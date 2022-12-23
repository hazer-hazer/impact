use std::collections::HashMap;

use crate::{
    ast::{
        expr::Block,
        item::{Item, ItemKind},
        pat::{Pat, PatKind},
        visitor::{walk_each_pr, AstVisitor},
        ErrorNode, NodeId, NodeMap, Path, WithNodeId, AST,
    },
    cli::verbose,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Span, Symbol},
};

use super::{
    def::{ModuleId, ROOT_DEF_ID, ROOT_MODULE_ID},
    res::{NamePath, Res},
};

#[derive(Debug)]
enum ScopeKind {
    Func,
    Module(ModuleId),
}

#[derive(Debug)]
struct Scope {
    kind: ScopeKind,
    locals: HashMap<Symbol, NodeId>,
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            locals: Default::default(),
        }
    }
}

pub struct NameResolver<'a> {
    ast: &'a AST,
    scopes: Vec<Scope>,
    nearest_mod_item: ModuleId, // Nearest `mod` item
    locals_spans: NodeMap<Span>,
    msg: MessageStorage,
    sess: Session,
}

impl<'a> NameResolver<'a> {
    pub fn new(sess: Session, ast: &'a AST) -> Self {
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

        let old_local = self.scope_mut().locals.insert(ident.sym(), node_id);

        if let Some(old_local) = old_local {
            MessageBuilder::error()
                .span(ident.span())
                .text(format!(
                    "Duplicate local variable `{}` definition",
                    ident.sym()
                ))
                .label(
                    self.locals_spans[&old_local],
                    "Previously defined here".to_string(),
                )
                .label(ident.span(), "Redefined here".to_string())
                .emit(self);
        } else {
            self.locals_spans.insert(node_id, ident.span());
        }
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn enter_module_scope(&mut self, module_id: ModuleId) {
        verbose!("Enter module scope {}", module_id);

        self.scopes.push(Scope::new(ScopeKind::Module(module_id)));
    }

    fn enter_func_scope(&mut self) {
        verbose!("Enter func scope");
        self.scopes.push(Scope::new(ScopeKind::Func));
    }

    fn exit_scope(&mut self) {
        verbose!("Exit scope");
        self.scopes.pop();
    }

    fn resolve_local(&mut self, name: &Ident) -> Option<Res> {
        let mut scope_id = self.scopes.len() - 1;
        loop {
            let local = &self.scopes[scope_id].locals.get(&name.sym());
            if let Some(&local) = local {
                return Some(Res::local(local));
            }
            if scope_id == 0 {
                break;
            }
            scope_id -= 1;
        }
        None
    }

    fn resolve_path(&mut self, path: &Path) -> Res {
        let segments = path.segments();

        // TODO: When generics added, don't resolve local if segment has generics
        if segments.len() == 1 {
            // TODO: Assert that segment is lowercase for local variable?
            if let Some(local) = self.resolve_local(&segments[0]) {
                return local;
            }
        }

        let mut search_mod = self.sess.def_table.get_module(self.nearest_mod_item);

        for seg_index in 0..segments.len() {
            let seg = segments[seg_index];
            let seg_name = seg.sym();
            let is_target = seg_index == segments.len() - 1;

            // Path prefix segments must be type identifiers
            if !is_target && !seg.is_ty() {
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

            let def = match search_mod.get_by_ident(&seg) {
                Some(def) => def,
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
                return Res::def(*def);
            } else {
                search_mod = self.sess.def_table.get_module(ModuleId::Module(*def))
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

impl<'a> AstVisitor for NameResolver<'a> {
    fn visit_err(&mut self, _: &ErrorNode) {}

    // visit_let: We don't have locals for now

    fn visit_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::Mod(name, items) => {
                let module_id =
                    ModuleId::Module(self.sess.def_table.get_def_id(item.id()).unwrap());
                self.nearest_mod_item = module_id;
                self.enter_module_scope(module_id);
                self.visit_mod_item(name, items, item.id());
                self.exit_scope();
            },
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty, item.id()),
            ItemKind::Decl(name, params, body) => {
                self.enter_func_scope();

                self.visit_decl_item(name, params, body, item.id());

                self.exit_scope();

                if params.is_empty() {
                    self.define_var(item.id(), item.name().unwrap());
                }
            },
        }
    }

    fn visit_pat(&mut self, pat: &Pat) {
        match pat.kind() {
            PatKind::Ident(ident) => self.define_var(pat.id(), ident.as_ref().unwrap()),
        }
    }

    fn visit_block(&mut self, block: &Block) {
        self.enter_module_scope(ModuleId::Block(block.id()));
        walk_each_pr!(self, block.stmts(), visit_stmt);
        self.exit_scope();
    }

    fn visit_path(&mut self, path: &Path) {
        let res = self.resolve_path(path);

        verbose!("Resolved path `{}` as {}", path, res);

        self.sess.res.set(NamePath::new(path.id()), res);
    }
}

impl<'a> Stage<()> for NameResolver<'a> {
    fn run(mut self) -> StageOutput<()> {
        self.enter_module_scope(ROOT_MODULE_ID);
        self.visit_ast(self.ast);
        StageOutput::new(self.sess, (), self.msg)
    }
}
