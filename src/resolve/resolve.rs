use std::collections::HashMap;

use super::{
    builtin::Builtin,
    def::{DefId, ModuleId, Namespace, ROOT_DEF_ID, ROOT_MODULE_ID},
    res::{NamePath, Res},
};
use crate::{
    ast::{
        expr::{Block, Call, Expr, ExprKind, Lit, PathExpr},
        item::{GenericParams, Item, ItemKind, Variant},
        pat::{Pat, PatKind},
        ty::{Ty, TyKind, TyPath},
        visitor::{walk_each_pr, walk_pr, AstVisitor},
        ErrorNode, IdentNode, NodeId, NodeMap, Path, WithNodeId, AST, N, PR,
    },
    cli::verbose,
    message::message::{impl_message_holder, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{
        builtin::{TyBuiltin, ValueBuiltin},
        def::DefKind,
        res::ResKind,
    },
    session::{stage_result, Session, Stage, StageResult},
    span::{
        sym::{Ident, Symbol},
        Span, WithSpan,
    },
};

#[derive(Debug)]
enum Scope {
    Local(HashMap<Symbol, NodeId>),
    Module(ModuleId),
}

impl Scope {
    pub fn add_local(&mut self, name: Symbol, node_id: NodeId) -> Option<NodeId> {
        match self {
            Self::Local(locals) => locals.insert(name, node_id),
            _ => panic!("Cannot add local to non-local scope"),
        }
    }
}

pub struct NameResolver<'ast> {
    ast: &'ast AST,
    scopes: Vec<Scope>,
    /// Nearest `mod` item
    nearest_mod_item: ModuleId,
    locals_spans: NodeMap<Span>,
    msg: MessageStorage,
    sess: Session,
}

impl_message_holder!(NameResolver<'a>);

impl<'ast> NameResolver<'ast> {
    pub fn new(sess: Session, ast: &'ast AST) -> Self {
        Self {
            ast,
            scopes: Default::default(),
            nearest_mod_item: ModuleId::Def(ROOT_DEF_ID),
            locals_spans: Default::default(),
            msg: Default::default(),
            sess,
        }
    }

    fn define_local(&mut self, node_id: NodeId, ident: &Ident) {
        verbose!("Define var {} {}", node_id, ident);

        match self.scope() {
            &Scope::Module(module_id) => {
                // If we're not in block, then variable must defined as value in `DefCollector`
                assert!(
                    self.sess
                        .def_table
                        .get_module(module_id)
                        .get_from_ns(Namespace::Value, ident)
                        .is_some(),
                    "Value {} must be defined in Module scope",
                    ident
                );
                return;
            },
            Scope::Local(_) => {},
        }

        let old_local = self.scope_mut().add_local(ident.sym(), node_id);

        if let Some(old_local) = old_local {
            MessageBuilder::error()
                .span(ident.span())
                .text(format!(
                    "Duplicate local variable `{}` definition",
                    ident.sym()
                ))
                .label(
                    *self.locals_spans.get_unwrap(old_local),
                    "Previously defined here".to_string(),
                )
                .label(ident.span(), "Redefined here".to_string())
                .emit(self);
        } else {
            self.sess.def_table.define(node_id, DefKind::Local, ident);
            self.locals_spans.insert(node_id, ident.span());
        }
    }

    fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn in_local_scope(&self) -> bool {
        matches!(self.scope(), Scope::Local(_))
    }

    fn enter_module_scope(&mut self, module_id: ModuleId) {
        verbose!(
            "Enter module scope {} {:?}",
            module_id,
            self.sess.def_table.get_module(module_id)
        );

        self.scopes.push(Scope::Module(module_id));
    }

    fn enter_local_scope(&mut self) {
        self.scopes.push(Scope::Local(Default::default()));
    }

    fn exit_scope(&mut self) {
        verbose!("Exit scope");
        self.scopes.pop();
    }

    fn def_res(&self, target_ns: Namespace, def_id: DefId) -> Res {
        let def = self.sess.def_table.get_def(def_id);

        match def.kind() {
            DefKind::Lambda
            | DefKind::Root
            | DefKind::TyAlias
            | DefKind::Mod
            | DefKind::Func
            | DefKind::External
            | DefKind::Adt
            | DefKind::Variant
            | DefKind::Ctor
            | DefKind::FieldAccessor
            | DefKind::Value
            | DefKind::TyParam => {
                assert_eq!(def.kind().namespace(), target_ns);
                return Res::def(def_id);
            },
            DefKind::DeclareBuiltin => {
                // Is does not matter in which namespace we found `builtin`
                // Yeah, crutches
                return Res::declare_builtin();
            },
            DefKind::Local => unreachable!(),
        }
    }

    /// Try to find a local variable or ascend scopes looking for a name
    fn resolve_relative(&mut self, target_ns: Namespace, name: &Ident) -> Res {
        // TODO: When generics added, don't resolve local if segment has generics

        self.scopes
            .iter()
            .rev()
            .find_map(|scope| {
                match scope {
                    Scope::Local(locals) if target_ns == Namespace::Value => {
                        let local = &locals.get(&name.sym());
                        if let Some(&local) = local {
                            Some(Res::local(local))
                        } else {
                            None
                        }
                    },
                    Scope::Local(_) => None,
                    &Scope::Module(module_id) => {
                        // TODO: Check def kind
                        if let Some(def_id) = self
                            .sess
                            .def_table
                            .get_module(module_id)
                            .get_from_ns(target_ns, name)
                        {
                            Some(self.def_res(target_ns, def_id))
                        } else {
                            None
                        }
                    },
                }
            })
            .unwrap_or_else(|| {
                MessageBuilder::error()
                    .span(name.span())
                    .text(format!("`{}` is not defined in current scope", name))
                    .emit_single_label(self);
                Res::error()
            })
    }

    fn resolve_module_relative(&mut self, name: &Ident, path: &Path) -> Option<ModuleId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| {
                match scope {
                    Scope::Local(_) => None,
                    &Scope::Module(module_id) => {
                        // TODO: Check def kind
                        if let Some(def_id) = self
                            .sess
                            .def_table
                            .get_module(module_id)
                            .get_from_ns(Namespace::Type, name)
                        {
                            Some(def_id)
                        } else {
                            None
                        }
                    },
                }
            })
            .and_then(|def_id| {
                // TODO: Alternatives
                let def = self.sess.def_table.get_def(def_id);
                match def.kind() {
                    // TODO: Review Adt + Variant as module
                    DefKind::Adt | DefKind::Variant | DefKind::Mod => Some(ModuleId::Def(def_id)),
                    DefKind::TyAlias => {
                        MessageBuilder::error()
                            .span(path.span())
                            .text(format!("{} `{}` is not a module", def.kind(), def.name()))
                            .emit_single_label(self);
                        None
                    },
                    // FIXME: Really unreachable?
                    DefKind::Root
                    | DefKind::Ctor
                    | DefKind::FieldAccessor
                    | DefKind::Func
                    | DefKind::Value
                    | DefKind::Lambda
                    | DefKind::Local
                    | DefKind::External
                    | DefKind::TyParam
                    | DefKind::DeclareBuiltin => unreachable!(),
                }
            })
    }

    /// Resolution strategy is different for single-segment paths
    /// and multiple-segment paths.
    fn resolve_path(&mut self, target_ns: Namespace, path: &Path) -> Res {
        if path.segments().len() == 1 {
            // TODO: Assert that segment is lowercase for local variable?
            self.resolve_relative(target_ns, path.segments()[0].expect_name())
        } else {
            self.resolve_path_absolute(target_ns, path)
        }
    }

    fn resolve_member(&mut self, field: &Ident) -> Res {
        self.resolve_relative(Namespace::Value, field)
    }

    /// Relatively find definition of first segment then go by modules rest
    /// segments point to
    fn resolve_path_absolute(&mut self, target_ns: Namespace, path: &Path) -> Res {
        let search_mod =
            self.resolve_module_relative(path.segments().first().unwrap().expect_name(), path);

        let mut search_mod = if let Some(search_mod) = search_mod {
            self.sess.def_table.get_module(search_mod)
        } else {
            return Res::error();
        };

        for (index, seg) in path.segments().iter().enumerate().skip(1) {
            let is_target = index == path.segments().len() - 1;
            let name = *seg.expect_name();
            let span = seg.span();
            let prefix_str = &path.prefix_str(index);
            let prefix_span = path.prefix_span(index);

            dbg!(is_target, name, span, prefix_str);

            // Path prefix segments must be type identifiers
            if !is_target && !seg.expect_name().is_ty() {
                panic!("Must be check in ASTValidator");
                // MessageBuilder::error()
                //     .span(prefix_span)
                //     .text(format!("Invalid path `{}`", path))
                //     .label(
                //         seg.span(),
                //         format!("{} must be a type or module name",
                // seg_name),     )
                //     .emit(self);
                // return Res::error();
            }

            let ns = if is_target {
                target_ns
            } else {
                Namespace::Type
            };

            let def_id = match search_mod.get_from_ns(ns, &name) {
                Some(def_id) => def_id,
                None => {
                    verbose!("Def {} not found in {:?}", name, search_mod);
                    MessageBuilder::error()
                        .span(prefix_span)
                        .text(format!("Cannot find `{}` in {}", name, prefix_str))
                        .label(span, format!("`{}` is not defined in {}", name, prefix_str))
                        .emit(self);
                    return Res::error();
                },
            };

            if is_target {
                return self.def_res(target_ns, def_id);
            } else {
                search_mod = self.sess.def_table.get_module(ModuleId::Def(def_id))
            }
        }

        unreachable!()
    }

    fn maybe_builtin(&mut self, path: &Path, args: &[&Expr]) -> Option<Builtin> {
        // Note: All paths in declaration body must already be resolved
        let res = self.sess.res.get(NamePath::new(path.id())).unwrap();
        if let ResKind::DeclareBuiltin = res.kind() {
            if args.len() != 1 {
                MessageBuilder::error()
                    .span(path.span())
                    .text(format!(
                        "`builtin` expects exactly one argument, but {} were supplied",
                        args.len()
                    ))
                    .emit_single_label(self);
                return None;
            }

            let builtin_name_arg = args[0];
            let arg_span = builtin_name_arg.span();

            let name = match builtin_name_arg.kind() {
                ExprKind::Lit(lit) => match lit {
                    Lit::String(name) => Some(name),
                    _ => None,
                },
                _ => None,
            };
            if let Some(name) = name {
                if let Ok(bt) = Builtin::try_from(name.as_str()) {
                    Some(bt)
                } else {
                    MessageBuilder::error()
                        .text(format!("Unknown builtin `{}`", name))
                        .span(arg_span)
                        .emit_single_label(self);
                    None
                }
            } else {
                MessageBuilder::error()
                    .text("`builtin` expects string literal (builtin name) as argument".to_string())
                    .span(arg_span)
                    .emit_single_label(self);
                None
            }
        } else {
            None
        }
    }
}

impl<'ast> AstVisitor<'ast> for NameResolver<'ast> {
    fn visit_err(&mut self, _: &'ast ErrorNode) {}

    fn visit_item(&mut self, item: &'ast Item) {
        match item.kind() {
            ItemKind::Decl(name, params, body) if params.is_empty() => {
                self.define_local(item.id(), name.as_ref().unwrap());
                self.visit_decl_item(name, params, body, item.id());
                return;
            },
            ItemKind::Extern(items) => return self.visit_extern_block(items),
            _ => {},
        }

        let def_id = self
            .sess
            .def_table
            .get_def_id(item.id())
            .expect(&format!("Expected DefId for NodeId{}", item.id()));
        self.enter_module_scope(ModuleId::Def(def_id));

        match item.kind() {
            ItemKind::Mod(name, items) => {
                self.nearest_mod_item = ModuleId::Def(def_id);
                self.visit_mod_item(name, items, item.id());
            },
            ItemKind::Type(name, generics, ty) => self.visit_ty_item(name, generics, ty, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id());
            },
            ItemKind::Adt(name, generics, variants) => {
                self.visit_adt_item(name, generics, variants, item.id())
            },
            ItemKind::Extern(_) => unreachable!(),
        }

        self.exit_scope();
    }

    fn visit_decl_item(
        &mut self,
        name: &'ast PR<Ident>,
        params: &'ast Vec<PR<Pat>>,
        body: &'ast PR<N<Expr>>,
        id: NodeId,
    ) {
        walk_pr!(self, name, visit_ident);

        self.enter_local_scope();

        walk_each_pr!(self, params, visit_pat);
        walk_pr!(self, body, visit_expr);

        self.exit_scope();

        if params.is_empty() {
            if let ExprKind::Call(Call { lhs, args }) = body.as_deref().unwrap().kind() {
                if let ExprKind::Path(PathExpr(path)) = lhs.as_deref().unwrap().kind() {
                    let path = path.as_ref().unwrap();
                    let maybe_builtin = self.maybe_builtin(
                        path,
                        &args
                            .iter()
                            .map(|arg| arg.as_deref().unwrap())
                            .collect::<Vec<_>>(),
                    );

                    if let Some(bt) = maybe_builtin {
                        let bt_expr: Result<ValueBuiltin, _> = bt.try_into();
                        if let Ok(bt_expr) = bt_expr {
                            self.sess.def_table.add_builtin(
                                bt_expr.into(),
                                self.sess.def_table.get_def_id(id).unwrap(),
                            )
                        } else {
                            MessageBuilder::error()
                                .text(format!("{} builtin cannot be used as value", bt))
                                .span(args.first().unwrap().span())
                                .emit_single_label(self);
                        }
                    }
                }
            }
        }
    }

    fn visit_ty_item(
        &mut self,
        name: &'ast PR<Ident>,
        generics: &'ast GenericParams,
        ty: &'ast PR<N<Ty>>,
        id: NodeId,
    ) {
        walk_pr!(self, name, visit_ident);
        self.visit_generic_params(generics);
        walk_pr!(self, ty, visit_ty);

        if let TyKind::AppExpr(lhs, args) = ty.as_deref().unwrap().kind() {
            if let TyKind::Path(TyPath(path)) = lhs.as_deref().unwrap().kind() {
                let path = path.as_ref().unwrap();
                let maybe_builtin = self.maybe_builtin(
                    path,
                    &args
                        .iter()
                        .map(|arg| arg.as_deref().unwrap())
                        .collect::<Vec<_>>(),
                );

                if let Some(bt) = maybe_builtin {
                    let bt_ty: Result<TyBuiltin, _> = bt.try_into();
                    if let Ok(bt_expr) = bt_ty {
                        self.sess.def_table.add_builtin(
                            bt_expr.into(),
                            self.sess.def_table.get_def_id(id).unwrap(),
                        )
                    } else {
                        MessageBuilder::error()
                            .text(format!("{} builtin cannot be used as type", bt))
                            .span(args.first().unwrap().span())
                            .emit_single_label(self);
                    }
                    // Continue lowering even an error appeared.
                    // Lowering methods do not handle Results :(
                }
            }
        }
    }

    fn visit_variant(&mut self, variant: &'ast Variant) {
        self.enter_module_scope(ModuleId::Def(
            self.sess.def_table.get_def_id(variant.id()).unwrap(),
        ));
        walk_each_pr!(self, &variant.fields, visit_field);
        self.exit_scope();
    }

    fn visit_pat(&mut self, pat: &'ast Pat) {
        verbose!("Visit pat {}", pat.id());
        match pat.kind() {
            PatKind::Unit => {},
            PatKind::Ident(ident) => self.define_local(pat.id(), ident.as_ref().unwrap()),
        }
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.enter_module_scope(ModuleId::Block(block.id()));
        self.enter_local_scope();
        walk_each_pr!(self, block.stmts(), visit_stmt);
        self.exit_scope();
        self.exit_scope();
    }

    fn visit_path_expr(&mut self, path: &'ast PathExpr) {
        let path = path.0.as_ref().unwrap();
        let res = self.resolve_path(Namespace::Value, path);

        self.sess.res.set(NamePath::new(path.id()), res);
    }

    fn visit_ty_path(&mut self, path: &'ast TyPath) {
        let path = path.0.as_ref().unwrap();
        let res = self.resolve_path(Namespace::Type, path);

        self.sess.res.set(NamePath::new(path.id()), res);
    }

    // FIXME: Should resolve to field accessor NodeId
    fn visit_dot_op_expr(&mut self, lhs: &'ast PR<N<Expr>>, field: &'ast PR<IdentNode>) {
        walk_pr!(self, lhs, visit_expr);
        let field = field.as_ref().unwrap();
        let res = self.resolve_member(&field.ident);
        self.sess.res.set(NamePath::new(field.id()), res);
    }

    fn visit_path(&mut self, _: &'ast Path) {
        unreachable!()
    }
}

impl<'ast> Stage<()> for NameResolver<'ast> {
    fn run(mut self) -> StageResult<()> {
        self.enter_module_scope(ROOT_MODULE_ID);
        self.visit_ast(self.ast);
        stage_result(self.sess, (), self.msg)
    }
}
