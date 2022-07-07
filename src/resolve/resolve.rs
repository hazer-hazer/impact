use std::collections::HashMap;

use crate::{
    ast::{
        expr::Block,
        item::{Item, ItemKind},
        visitor::{walk_each_pr, AstVisitor},
        ErrorNode, NodeId, NodeMap, Path, WithNodeId, AST,
    },
    cli::verbose,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Span, Symbol},
};

use super::{
    def::{ModuleId, ROOT_DEF_ID},
    res::{NamePath, Res},
};

enum ScopeKind {
    Block,
    Module(ModuleId),
}

struct Scope {
    locals: HashMap<Symbol, NodeId>,
    kind: ScopeKind,
}

impl Scope {
    pub fn new_module(module_id: ModuleId) -> Self {
        Self {
            locals: Default::default(),
            kind: ScopeKind::Module(module_id),
        }
    }

    pub fn new_block() -> Self {
        Self {
            locals: Default::default(),
            kind: ScopeKind::Block,
        }
    }

    pub fn get(&self, sym: Symbol) -> Option<&NodeId> {
        self.locals.get(&sym)
    }

    pub fn define(&mut self, sym: Symbol, node_id: NodeId) -> Option<NodeId> {
        self.locals.insert(sym, node_id)
    }
}

pub struct NameResolver<'a> {
    ast: &'a AST,
    scopes: Vec<Scope>,
    nearest_mod: ModuleId, // Nearest `mod` item
    locals_spans: NodeMap<Span>,
    msg: MessageStorage,
    sess: Session,
}

impl<'a> NameResolver<'a> {
    pub fn new(sess: Session, ast: &'a AST) -> Self {
        Self {
            ast,
            scopes: Default::default(),
            nearest_mod: ModuleId::Module(ROOT_DEF_ID),
            locals_spans: Default::default(),
            msg: Default::default(),
            sess,
        }
    }

    fn enter_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn define_local(&mut self, ident: Ident, node_id: NodeId) {
        let defined = self.scope().define(ident.sym(), node_id);
        if let Some(old) = defined {
            MessageBuilder::error()
                .span(self.locals_spans[&old])
                .text(format!("{} is already defined", ident))
                .emit(self);
        } else {
            self.locals_spans.insert(node_id, ident.span());
        }
    }

    fn resolve_path(&mut self, path: &Path) -> Res {
        let segments = path.segments();

        if segments.get(0).unwrap().is_var() && segments.len() == 1 {
            let seg = segments.get(0).unwrap();
            if let Some(local) = self.scope().get(seg.sym()) {
                return Res::local(*local);
            } else {
                return Res::error();
            }
        }

        // Path is not a path if it is not a path ☝️
        assert!(segments.get(0).unwrap().is_ty());

        let mut search_mod = self.sess.def_table.get_module(self.nearest_mod);

        for seg_index in 0..segments.len() {
            let seg = segments[seg_index];
            let seg_name = seg.sym();
            let is_target = seg_index == segments.len() - 1;

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
                    MessageBuilder::error()
                        .span(path.prefix_span(seg_index))
                        .text(format!(
                            "Cannot find {} in {}",
                            seg_name,
                            path.prefix_str(seg_index)
                        ))
                        .emit_single_label(self);
                    return Res::error();
                }
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
                self.nearest_mod = module_id;
                self.enter_scope(Scope::new_module(module_id));
                self.visit_mod_item(name, items);
                self.exit_scope();
            }
            ItemKind::Type(_, _) => {}
            ItemKind::Decl(name, params, body) => self.visit_decl_item(name, params, body),
        }
    }

    fn visit_block(&mut self, block: &Block) {
        self.enter_scope(Scope::new_block());
        walk_each_pr!(self, block.stmts(), visit_stmt);
        self.exit_scope();
    }

    fn visit_path(&mut self, path: &Path) {
        verbose!("Resolve path `{}`", path);

        let res = self.resolve_path(path);
        self.sess.res.set(NamePath::new(path.id()), res);
    }
}

impl<'a> Stage<()> for NameResolver<'a> {
    fn run(mut self) -> StageOutput<()> {
        self.visit_ast(self.ast);
        StageOutput::new(self.sess, (), self.msg)
    }
}
