use crate::{
    ast::{
        self,
        expr::{Block, Expr, ExprKind, InfixOp, Lit, PrefixOp},
        item::{Item, ItemKind},
        ty::Ty,
        visitor::{visit_each_pr, visit_pr, AstVisitor},
        ErrorNode, NodeId, AST, N, PR,
    },
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    session::{Session, Stage, StageOutput},
    span::span::Ident,
};

use super::def::{DefId, DefKind, Module, ModuleId, ROOT_DEF_ID};

pub struct DefCollector<'a> {
    sess: Session,
    ast: &'a AST,
    current_module: ModuleId,
    msg: MessageStorage,
}

impl<'a> MessageHolder for DefCollector<'a> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'a> DefCollector<'a> {
    pub fn new(sess: Session, ast: &'a AST) -> Self {
        Self {
            sess,
            ast,
            current_module: ModuleId::Module(ROOT_DEF_ID),
            msg: Default::default(),
        }
    }

    fn module(&mut self) -> &mut Module {
        self.sess.def_table.get_module_mut(self.current_module)
    }

    fn enter_def_module(&mut self, def_id: DefId) {
        self.current_module = self.sess.def_table.add_module(def_id, self.current_module);
    }

    fn enter_block_module(&mut self, node_id: NodeId) {
        self.current_module = self.sess.def_table.add_block(node_id, self.current_module)
    }

    fn exit_module(&mut self) {
        self.current_module = self
            .sess
            .def_table
            .get_module(self.current_module)
            .parent()
            .expect("Tried to exit root module")
    }

    fn define(&mut self, node_id: NodeId, kind: DefKind, ident: &Ident) -> DefId {
        let def_id = self.sess.def_table.define(node_id, kind, ident);
        let old_def = self.module().define(kind.namespace(), ident.sym(), def_id);

        if let Some(old_def) = old_def {
            let old_def = self.sess.def_table.get_def(old_def).unwrap();
            MessageBuilder::error()
                .span(ident.span())
                .text(format!("Tried to redefine `{}`", ident.sym()))
                .label(old_def.name().span(), "Previously defined here".to_string())
                .label(ident.span(), "Redefined here".to_string())
                .emit(self);
        }

        def_id
    }
}

impl<'a> AstVisitor<()> for DefCollector<'a> {
    fn visit_err(&self, _: &ErrorNode) {}

    fn visit_ast(&mut self, ast: &AST) {
        self.sess.def_table.add_root_module();
        visit_each_pr!(self, ast.items(), visit_item);
    }

    // Items //
    fn visit_item(&mut self, item: &Item) {
        let def_id = self.define(
            item.id(),
            DefKind::from_item_kind(item.kind()),
            item.name()
                .expect("Cannot define unnamed item. TODO: Synthesize unnamed item name"),
        );

        match item.kind() {
            ItemKind::Mod(name, items) => {
                self.enter_def_module(def_id);
                self.visit_mod_item(name, items);
                self.exit_module();
            }
            ItemKind::Type(_, _) => {}
            ItemKind::Decl(name, params, body) => self.visit_decl_item(name, params, body),
        }
    }

    fn visit_type_item(&mut self, _: &PR<Ident>, _: &PR<N<Ty>>) {}

    fn visit_mod_item(&mut self, _: &PR<Ident>, items: &Vec<PR<N<Item>>>) {
        visit_each_pr!(self, items, visit_item);
    }

    fn visit_decl_item(&mut self, _: &PR<Ident>, _: &Vec<PR<Ident>>, body: &PR<N<Expr>>) {
        visit_pr!(self, body, visit_expr)
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) {
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Infix(lhs, op, rhs) => self.visit_infix_expr(lhs, op, rhs),
            ExprKind::Prefix(op, rhs) => self.visit_prefix_expr(op, rhs),
            ExprKind::App(lhs, arg) => self.visit_app_expr(lhs, arg),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Abs(param, body) => self.visit_abs_expr(param, body),
            ExprKind::Ty(expr, ty) => self.visit_type_expr(expr, ty),
        }
    }

    fn visit_lit_expr(&mut self, _: &Lit) {}

    fn visit_ident(&mut self, _: &Ident) {}

    fn visit_infix_expr(&mut self, lhs: &PR<N<Expr>>, _: &InfixOp, rhs: &PR<N<Expr>>) {
        visit_pr!(self, lhs, visit_expr);
        visit_pr!(self, rhs, visit_expr);
    }

    fn visit_prefix_expr(&mut self, _: &PrefixOp, rhs: &PR<N<Expr>>) {
        visit_pr!(self, rhs, visit_expr);
    }

    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) {
        visit_pr!(self, lhs, visit_expr);
        visit_pr!(self, arg, visit_expr);
    }

    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) {
        visit_pr!(self, param, visit_ident);
        visit_pr!(self, body, visit_expr);
    }

    fn visit_let_expr(&mut self, block: &PR<Block>) {
        visit_pr!(self, block, visit_block);
    }

    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) {
        visit_pr!(self, expr, visit_expr);
        visit_pr!(self, ty, visit_ty);
    }

    // Types //
    fn visit_unit_ty(&mut self) {}

    fn visit_lit_ty(&mut self, _: &ast::ty::LitTy) {}

    fn visit_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) {
        visit_pr!(self, param_ty, visit_ty);
        visit_pr!(self, return_ty, visit_ty);
    }

    fn visit_path(&mut self, _: &ast::Path) {}

    fn visit_block(&mut self, block: &ast::expr::Block) -> () {
        self.enter_block_module(block.id());
        visit_each_pr!(self, block.stmts(), visit_stmt);
        visit_pr!(self, block.expr(), visit_expr);
        self.exit_module();
    }
}

impl<'a> Stage<()> for DefCollector<'a> {
    fn run(mut self) -> StageOutput<()> {
        self.visit_ast(self.ast);
        StageOutput::new(self.sess, (), self.msg)
    }
}
