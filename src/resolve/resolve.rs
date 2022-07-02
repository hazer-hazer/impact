use std::collections::HashMap;

use crate::{
    ast::{
        expr::Expr,
        item::Item,
        ty::Ty,
        visitor::{walk_each_pr, walk_pr, AstVisitor},
        ErrorNode, NodeId, Path, AST, N, PR, NodeMap,
    },
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Symbol, Span},
};

use super::{
    def::{ModuleId, ROOT_DEF_ID},
    res::Res,
};

enum RibKind {
    Block,
    Module(ModuleId),
}

struct Rib {
    locals: HashMap<Symbol, NodeId>,
    kind: RibKind,
}

impl Rib {
    pub fn new_module(module_id: ModuleId) -> Self {
        Self { locals: Default::default(), kind: RibKind::Module(module_id) }
    }

    pub fn new_block() -> Self {
        Self { locals: Default::default(), kind: RibKind::Block }
    }

    pub fn get(&self, sym: Symbol) -> Option<&NodeId> {
        self.locals.get(&sym)
    }

    pub fn define(&self, sym: Symbol, node_id: NodeId) -> Option<NodeId> {
        self.locals.insert(sym, node_id)
    }
}

pub struct NameResolver<'a> {
    ast: &'a AST,
    ribs: Vec<Rib>,
    nearest_mod: ModuleId,
    locals_span: NodeMap<Span>,
    msg: MessageStorage,
    sess: Session,
}

impl<'a> NameResolver<'a> {
    pub fn new(sess: Session, ast: &'a AST) -> Self {
        Self {
            ast,
            ribs: Default::default(),
            nearest_mod: ModuleId::Module(ROOT_DEF_ID),
            locals_span: Default::default(),
            msg: Default::default(),
            sess,
        }
    }

    fn rib(&self) -> &Rib {
        &self.ribs.last().unwrap()
    }

    fn define_local(&self, sym: Symbol, node_id: NodeId) {
        if let Some(old) = self.rib().define(sym, node_id) {
            
        }
    }

    fn resolve_path(&mut self, path: &Path) -> Res {
        let segments = path.segments();

        if segments.get(0).unwrap().is_var() && segments.len() == 0 {
            let seg = segments.get(0).unwrap();
            if let Some(local) = self.rib().get(seg.sym()) {
                return Res::local(*local);
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
    fn visit_err(&self, _: &ErrorNode) {}

    fn visit_type_item(&mut self, _: &PR<Ident>, ty: &PR<N<Ty>>) -> () {
        walk_pr!(self, ty, visit_ty);
    }

    fn visit_mod_item(&mut self, _: &PR<Ident>, items: &Vec<PR<N<Item>>>) -> () {
        walk_each_pr!(self, items, visit_item)
    }

    fn visit_decl_item(
        &mut self,
        name: &PR<Ident>,
        params: &Vec<PR<Ident>>,
        body: &PR<N<Expr>>,
    ) -> () {
    }
}

impl<'a> Stage<()> for NameResolver<'a> {
    fn run(self) -> StageOutput<()> {
        StageOutput::new(self.sess, (), self.msg)
    }
}
