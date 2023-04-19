use std::collections::HashMap;

use super::{
    def::{DefId, ModuleId, Namespace, ROOT_DEF_ID, ROOT_MODULE_ID},
    res::{NamePath, Res},
};
use crate::{
    ast::{
        expr::{Block, Expr, PathExpr},
        item::{Item, ItemKind},
        pat::{Pat, PatKind},
        ty::TyPath,
        visitor::{walk_each_pr, walk_pr, AstVisitor},
        ErrorNode, IdentNode, NodeId, NodeMap, Path, WithNodeId, AST, N, PR,
    },
    cli::verbose,
    message::message::{MessageBuilder, MessageHolder, MessageStorage},
    resolve::def::DefKind,
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
                match def.kind {
                    // TODO: Review Adt + Variant as module
                    DefKind::Adt | DefKind::Variant | DefKind::Mod => Some(ModuleId::Def(def_id)),
                    DefKind::TyAlias => {
                        MessageBuilder::error()
                            .span(path.span())
                            .text(format!("{} `{}` is not a module", def.kind, def.name))
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

        for (index, seg) in path.segments().iter().enumerate() {
            let is_target = index == path.segments().len() - 1;
            let name = *seg.expect_name();
            let span = seg.span();
            let prefix_str = &path.prefix_str(index);
            let prefix_span = path.prefix_span(index);

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
}

impl<'a> MessageHolder for NameResolver<'a> {
    fn storage(&mut self) -> &mut MessageStorage {
        &mut self.msg
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
            ItemKind::Type(name, generics, ty) => {
                self.visit_type_item(name, generics, ty, item.id())
            },
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
        _: NodeId,
    ) {
        walk_pr!(self, name, visit_ident);

        self.enter_local_scope();

        walk_each_pr!(self, params, visit_pat);
        walk_pr!(self, body, visit_expr);

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
