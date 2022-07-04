use crate::{
    ast::{
        expr::{InfixOp, Lit, PrefixOp},
        ty::LitTy,
    },
    span::span::Ident,
};

use super::{
    expr::{Block, Expr, ExprKind},
    item::{Item, ItemKind},
    stmt::{Stmt, StmtKind},
    ty::{Ty, TyKind},
    Path, HIR, N,
};

macro_rules! walk_each {
    ($self: ident, $els: expr, $visitor: ident) => {
        for el in $els {
            $self.$visitor(el);
        }
    };
}

pub trait HirVisitor {
    fn visit_hir(&mut self, hir: &HIR) {
        walk_each!(self, hir.items(), visit_item);
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) {
        self.visit_expr(expr)
    }

    fn visit_item_stmt(&mut self, item: &Item) {
        self.visit_item(item)
    }

    // Items //
    fn visit_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty),
            ItemKind::Mod(name, items) => self.visit_mod_item(name, items),
            ItemKind::Decl(name, params, body) => self.visit_decl_item(name, params, body),
        }
    }

    fn visit_type_item(&mut self, name: &Ident, ty: &N<Ty>) {
        self.visit_ident(name);
        self.visit_ty(ty);
    }

    fn visit_mod_item(&mut self, name: &Ident, items: &Vec<Item>) {
        self.visit_ident(name);
        walk_each!(self, items, visit_item);
    }

    fn visit_decl_item(&mut self, name: &Ident, params: &Vec<Ident>, body: &Expr) {
        self.visit_ident(name);
        walk_each!(self, params, visit_ident);
        self.visit_expr(body);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) {
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(lhs, op, rhs) => self.visit_infix_expr(lhs, op, rhs),
            ExprKind::Prefix(op, rhs) => self.visit_prefix_expr(op, rhs),
            ExprKind::App(lhs, arg) => self.visit_app_expr(lhs, arg),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Abs(param, body) => self.visit_abs_expr(param, body),
            ExprKind::Ty(expr, ty) => self.visit_type_expr(expr, ty),
        }
    }

    fn visit_lit_expr(&mut self, lit: &Lit) {}

    fn visit_path_expr(&mut self, path: &Path) {
        self.visit_path(path)
    }

    fn visit_block_expr(&mut self, block: &Block) {
        self.visit_block(block)
    }

    fn visit_infix_expr(&mut self, lhs: &N<Expr>, op: &InfixOp, rhs: &N<Expr>) {
        self.visit_expr(lhs);
        self.visit_expr(rhs);
    }

    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &N<Expr>) {
        self.visit_expr(rhs);
    }

    fn visit_app_expr(&mut self, lhs: &N<Expr>, arg: &N<Expr>) {
        self.visit_expr(lhs);
        self.visit_expr(arg);
    }

    fn visit_abs_expr(&mut self, param: &Ident, body: &N<Expr>) {
        self.visit_ident(param);
        self.visit_expr(body);
    }

    fn visit_let_expr(&mut self, block: &Block) {
        self.visit_block(block);
    }

    fn visit_type_expr(&mut self, expr: &N<Expr>, ty: &Ty) {
        self.visit_expr(expr);
        self.visit_ty(ty);
    }

    // Types //
    fn visit_ty(&mut self, ty: &Ty) {
        match ty.kind() {
            TyKind::Unit => self.visit_unit_ty(),
            TyKind::Lit(lit_ty) => self.visit_lit_ty(lit_ty),
            TyKind::Path(path) => self.visit_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
        }
    }
    fn visit_unit_ty(&mut self) {}

    fn visit_lit_ty(&mut self, lit_ty: &LitTy) {}

    fn visit_path_ty(&mut self, path: &Path) {
        self.visit_path(path)
    }

    fn visit_func_ty(&mut self, param_ty: &N<Ty>, return_ty: &N<Ty>) {
        self.visit_ty(param_ty);
        self.visit_ty(return_ty);
    }

    // Fragments //
    fn visit_ident(&mut self, name: &Ident) {}

    fn visit_path(&mut self, path: &Path) {}

    fn visit_block(&mut self, block: &Block) {
        walk_each!(self, block.stmts(), visit_stmt);
    }
}
