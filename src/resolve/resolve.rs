use std::collections::HashMap;

use crate::{
    ast::{
        expr::{Block, PathExpr},
        item::{Item, ItemKind},
        pat::{Pat, PatKind},
        ty::TyPath,
        visitor::{walk_each_pr, AstVisitor},
        ErrorNode, NodeId, NodeMap, Path, WithNodeId, AST,
    },
    cli::verbose,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::def::DefKind,
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Span, Symbol, WithSpan},
};

use super::{
    def::{DefId, ModuleId, Namespace, ROOT_DEF_ID},
    res::{NamePath, Res},
};

#[derive(Debug)]
enum ScopeKind {
    Block(NodeId, HashMap<Symbol, NodeId>),
    Def(DefId),
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
            ScopeKind::Block(_, locals) => locals.insert(name, node_id),
            _ => panic!("Cannot add local to non-block scope"),
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
            nearest_mod_item: ModuleId::Module(ROOT_DEF_ID),
            locals_spans: Default::default(),
            msg: Default::default(),
            sess,
        }
    }

    fn define_var(&mut self, node_id: NodeId, ident: &Ident) {
        verbose!("Define var {} {}", node_id, ident);

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

    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn enter_def_scope(&mut self, def_id: DefId) {
        verbose!("Enter module scope {}", def_id);

        self.scopes.push(Scope::new(ScopeKind::Def(def_id)));
    }

    fn enter_block_scope(&mut self, node_id: NodeId) {
        verbose!("Enter block scope {}", node_id);

        self.scopes
            .push(Scope::new(ScopeKind::Block(node_id, Default::default())))
    }

    fn exit_scope(&mut self) {
        verbose!("Exit scope");
        self.scopes.pop();
    }

    fn resolve_local(&mut self, name: &Ident) -> Option<Res> {
        let mut scope_id = self.scopes.len() - 1;
        loop {
            match &self.scope_mut().kind {
                ScopeKind::Block(_, locals) => {
                    let local = &locals.get(&name.sym());
                    if let Some(&local) = local {
                        return Some(Res::local(local));
                    }
                },
                &ScopeKind::Def(mod_def_id) => {
                    // TODO: Check def kind
                    if let Some(def_id) = self
                        .sess
                        .def_table
                        .get_module(ModuleId::Module(mod_def_id))
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

        // TODO: When generics added, don't resolve local if segment has generics
        if target_ns == Namespace::Value && segments.len() == 1 {
            // TODO: Assert that segment is lowercase for local variable?
            if let Some(local) = self.resolve_local(segments[0].expect_name()) {
                return local;
            }
        }

        let mut search_mod = self.sess.def_table.get_module(self.nearest_mod_item);

        for seg_index in 0..segments.len() {
            let seg = &segments[seg_index];
            let seg_name = seg.expect_name().sym();
            let is_target = seg_index == segments.len() - 1;

            // Path prefix segments must be type identifiers
            if !is_target && !seg.expect_name().is_ty() {
                MessageBuilder::error()
                    .span(path.prefix_span(seg_index))
                    .text(format!("Invalid path `{}`", path))
                    .label(
                        seg.span(),
                        format!("{} must be a type or module name", seg_name),
                    )
                    .emit(self);
                return Res::error();
            }

            let ns = if is_target {
                target_ns
            } else {
                Namespace::Type
            };

            let def_id = match search_mod.get_from_ns(ns, seg.expect_name()) {
                Some(def_id) => def_id,
                None => {
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
                search_mod = self.sess.def_table.get_module(ModuleId::Module(def_id))
            }
        }

        unreachable!()
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
        match item.kind() {
            ItemKind::Mod(name, items) => {
                let def_id = self.sess.def_table.get_def_id(item.id()).unwrap();
                let module_id = ModuleId::Module(def_id);
                self.nearest_mod_item = module_id;
                self.enter_def_scope(def_id);
                self.visit_mod_item(name, items, item.id());
                self.exit_scope();
            },
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id());

                if params.is_empty() {
                    self.define_var(item.id(), name.as_ref().unwrap());
                }
            },
        }
    }

    fn visit_pat(&mut self, pat: &'ast Pat) {
        verbose!("Visit pat {}", pat.id());
        match pat.kind() {
            PatKind::Unit => {},
            PatKind::Ident(ident) => self.define_var(pat.id(), ident.as_ref().unwrap()),
        }
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.enter_block_scope(block.id());
        walk_each_pr!(self, block.stmts(), visit_stmt);
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
        self.enter_def_scope(ROOT_DEF_ID);
        self.visit_ast(self.ast);
        StageOutput::new(self.sess, (), self.msg)
    }
}
