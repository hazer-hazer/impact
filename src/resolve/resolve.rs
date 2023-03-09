use std::collections::HashMap;

use crate::{
    ast::{
        expr::{Block, Expr, PathExpr},
        item::{Item, ItemKind},
        pat::{Pat, PatKind},
        ty::TyPath,
        visitor::{walk_each_pr, walk_pr, AstVisitor},
        ErrorNode, NodeId, NodeMap, Path, WithNodeId, AST, N, PR,
    },
    cli::verbose,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::def::DefKind,
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Span, Symbol, WithSpan},
};

use super::{
    def::{DefId, Module, ModuleId, Namespace, ROOT_DEF_ID, ROOT_MODULE_ID},
    res::{NamePath, Res},
};

/// Resolution strategy is different for single-segment paths
/// and multiple-segment paths.
enum ResStrategy {
    /// Try to find a local variable or ascend scopes looking for a name
    Relative,

    /// Relatively find definition of first segment then go by modules rest segments point to
    Absolute,
}

struct SegInfo<'a> {
    index: usize,
    name: Ident,
    span: Span,
    is_target: bool,
    prefix: (&'a str, Span),
}

enum SegResResult<'a> {
    Ok(Res),
    Err(MessageBuilder),
    SearchMod(&'a Module),
}

trait ResolvePath {
    fn resolve<'a>(
        &self,
        search_mod: &'a Module,
        target_ns: Namespace,
        f: impl FnMut(SegInfo, &Module) -> SegResResult<'a>,
    ) -> Result<Res, MessageBuilder>;
}

impl ResolvePath for Path {
    fn resolve<'a>(
        &self,
        mut search_mod: &'a Module,
        target_ns: Namespace,
        mut f: impl FnMut(SegInfo, &'a Module) -> SegResResult<'a>,
    ) -> Result<Res, MessageBuilder> {
        for (index, seg) in self.segments().iter().enumerate() {
            let is_target = index == self.segments().len() - 1;
            let name = *seg.expect_name();
            let span = seg.span();
            let prefix_str = &self.prefix_str(index);
            let prefix_span = self.prefix_span(index);

            let seg_info = SegInfo {
                index,
                name,
                span,
                is_target,
                prefix: (prefix_str, prefix_span),
            };

            // Path prefix segments must be type identifiers
            if !is_target && !seg.expect_name().is_ty() {
                panic!("Must be check in ASTValidator");
                // MessageBuilder::error()
                //     .span(prefix_span)
                //     .text(format!("Invalid path `{}`", path))
                //     .label(
                //         seg.span(),
                //         format!("{} must be a type or module name", seg_name),
                //     )
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
                    return Err(MessageBuilder::error()
                        .span(prefix_span)
                        .text(format!("Cannot find `{}` in {}", name, prefix_str))
                        .label(span, format!("`{}` is not defined in {}", name, prefix_str)));
                },
            };

            let seg_res = f(seg_info, search_mod);

            match seg_res {
                SegResResult::Ok(res) => return Ok(res),
                SegResResult::Err(msg) => return Err(msg),
                SegResResult::SearchMod(new_mod) => search_mod = new_mod,
            }
        }

        unreachable!()
    }
}

#[derive(Debug)]
enum ScopeKind {
    Local(HashMap<Symbol, NodeId>),
    Module(ModuleId),
}

#[derive(Debug)]
struct Scope {
    kind: ScopeKind,
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self { kind }
    }

    pub fn add_local(&mut self, name: Symbol, node_id: NodeId) -> Option<NodeId> {
        match &mut self.kind {
            ScopeKind::Local(locals) => locals.insert(name, node_id),
            _ => panic!("Cannot add local to non-local scope"),
        }
    }
}

pub struct NameResolver<'ast> {
    ast: &'ast AST,
    scopes: Vec<Scope>,
    nearest_mod_item: ModuleId, // Nearest `mod` item
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

    fn define_var(&mut self, node_id: NodeId, ident: &Ident) {
        verbose!("Define var {} {}", node_id, ident);

        match self.scope().kind {
            ScopeKind::Module(module_id) => {
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
            ScopeKind::Local(_) => {},
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
            self.sess.def_table.define(node_id, DefKind::Value, ident);
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
        matches!(self.scope().kind, ScopeKind::Local(_))
    }

    fn enter_module_scope(&mut self, module_id: ModuleId) {
        verbose!(
            "Enter module scope {} {:?}",
            module_id,
            self.sess.def_table.get_module(module_id)
        );

        self.scopes.push(Scope::new(ScopeKind::Module(module_id)));
    }

    fn enter_local_scope(&mut self) {
        self.scopes
            .push(Scope::new(ScopeKind::Local(Default::default())))
    }

    fn exit_scope(&mut self) {
        verbose!("Exit scope");
        self.scopes.pop();
    }

    fn resolve_local(&mut self, name: &Ident) -> Option<Res> {
        let mut scope_id = self.scopes.len() - 1;
        loop {
            match &self.scope_mut().kind {
                ScopeKind::Local(locals) => {
                    let local = &locals.get(&name.sym());
                    if let Some(&local) = local {
                        return Some(Res::local(local));
                    }
                },
                &ScopeKind::Module(module_id) => {
                    // TODO: Check def kind
                    if let Some(def_id) = self
                        .sess
                        .def_table
                        .get_module(module_id)
                        .get_from_ns(Namespace::Value, name)
                    {
                        return Some(self.def_res(Namespace::Value, def_id));
                    }
                },
            }

            if scope_id == 0 {
                break;
            }
            scope_id -= 1;
        }
        None
    }

    fn def_res(&self, target_ns: Namespace, def_id: DefId) -> Res {
        let def = self.sess.def_table.get_def(def_id).unwrap();

        match def.kind() {
            DefKind::Lambda
            | DefKind::Root
            | DefKind::TyAlias
            | DefKind::Mod
            | DefKind::Func
            | DefKind::Value => {
                assert_eq!(def.kind().namespace(), target_ns);
                return Res::def(def_id);
            },
            DefKind::DeclareBuiltin => {
                // Is does not matter in which namespace we found `builtin`
                // Yeah, crutches
                return Res::declare_builtin();
            },
        }
    }

    fn resolve_path(&mut self, target_ns: Namespace, path: &Path) -> Res {
        let segments = path.segments();

        let mut search_mod = self.sess.def_table.get_module(self.nearest_mod_item);

        for seg_index in 0..segments.len() {
            let seg = &segments[seg_index];
            let seg_name = seg.expect_name().sym();
            let is_target = seg_index == segments.len() - 1;

            let ns = if is_target {
                target_ns
            } else {
                Namespace::Type
            };

            let def_id = match search_mod.get_from_ns(ns, seg.expect_name()) {
                Some(def_id) => def_id,
                None => {
                    verbose!("Def {} not found in {:?}", seg_name, search_mod);
                    let prefix = path.prefix_str(seg_index);
                    MessageBuilder::error()
                        .span(path.prefix_span(seg_index))
                        .text(format!("Cannot find `{}` in {}", seg_name, prefix))
                        .label(
                            seg.span(),
                            format!("`{}` is not defined in {}", seg_name, prefix),
                        )
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

    fn resolve_path_relative(&mut self, target_ns: Namespace, path: &Path) -> Res {
        assert!(path.segments().len() == 1);

        // TODO: When generics added, don't resolve local if segment has generics
        if self.in_local_scope() && target_ns == Namespace::Value {
            // TODO: Assert that segment is lowercase for local variable?
            if let Some(local) = self.resolve_local(path.segments()[0].expect_name()) {
                return local;
            }
        }

        path.resolve(
            self.sess.def_table.get_module(self.nearest_mod_item),
            target_ns,
            |seg_info, search_mod| {
                let SegInfo {
                    index,
                    name,
                    span,
                    is_target,
                    prefix,
                } = seg_info;

                todo!()
            },
        )
        .unwrap_or_else(|err| {
            err.emit(self);
            Res::error()
        })
    }

    fn resolve_path_absolute(&mut self, target_ns: Namespace, path: &Path) -> Res {
        todo!()
    }
}

impl<'a> MessageHolder for NameResolver<'a> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'ast> AstVisitor<'ast> for NameResolver<'ast> {
    fn visit_err(&mut self, _: &'ast ErrorNode) {}

    fn visit_item(&mut self, item: &'ast Item) {
        let def_id = self.sess.def_table.get_def_id(item.id()).unwrap();
        self.enter_module_scope(ModuleId::Def(def_id));

        match item.kind() {
            ItemKind::Mod(name, items) => {
                self.nearest_mod_item = ModuleId::Def(def_id);
                self.visit_mod_item(name, items, item.id());
            },
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id());
            },
        }

        self.exit_scope();

        match item.kind() {
            ItemKind::Decl(name, params, _) if params.is_empty() => {
                self.define_var(item.id(), name.as_ref().unwrap());
            },
            _ => {},
        }
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
            PatKind::Ident(ident) => self.define_var(pat.id(), ident.as_ref().unwrap()),
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

    fn visit_path(&mut self, _: &'ast Path) {
        unreachable!()
    }
}

impl<'ast> Stage<()> for NameResolver<'ast> {
    fn run(mut self) -> StageOutput<()> {
        self.enter_module_scope(ROOT_MODULE_ID);
        self.visit_ast(self.ast);
        StageOutput::new(self.sess, (), self.msg)
    }
}
