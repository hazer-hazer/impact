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

pub trait HirVisitor<T> {
    fn visit_hir(&mut self, hir: &HIR) -> T;

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) -> T {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> T {
        self.visit_expr(expr)
    }

    fn visit_item_stmt(&mut self, item: &Item) -> T {
        self.visit_item(item)
    }

    // Items //
    fn visit_item(&mut self, item: &Item) -> T {
        match item.kind() {
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty),
            ItemKind::Mod(name, items) => self.visit_mod_item(name, items),
            ItemKind::Decl(name, params, body) => self.visit_decl_item(name, params, body),
        }
    }

    fn visit_type_item(&mut self, name: &Ident, ty: &N<Ty>) -> T;

    fn visit_mod_item(&mut self, name: &Ident, items: &Vec<Item>) -> T;

    fn visit_decl_item(&mut self, name: &Ident, params: &Vec<Ident>, body: &Expr) -> T;

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) -> T {
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

    fn visit_lit_expr(&mut self, lit: &Lit) -> T;
    fn visit_path_expr(&mut self, path: &Path) -> T {
        self.visit_path(path)
    }
    fn visit_infix_expr(&mut self, lhs: &N<Expr>, op: &InfixOp, rhs: &N<Expr>) -> T;
    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &N<Expr>) -> T;
    fn visit_app_expr(&mut self, lhs: &N<Expr>, arg: &N<Expr>) -> T;
    fn visit_abs_expr(&mut self, param: &Ident, body: &N<Expr>) -> T;
    fn visit_let_expr(&mut self, block: &Block) -> T;
    fn visit_type_expr(&mut self, expr: &N<Expr>, ty: &Ty) -> T;

    // Types //
    fn visit_ty(&mut self, ty: &Ty) -> T {
        match ty.kind() {
            TyKind::Unit => self.visit_unit_ty(),
            TyKind::Lit(lit_ty) => self.visit_lit_ty(lit_ty),
            TyKind::Path(path) => self.visit_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
        }
    }
    fn visit_unit_ty(&mut self) -> T;
    fn visit_lit_ty(&mut self, lit_ty: &LitTy) -> T;
    fn visit_path_ty(&mut self, path: &Path) -> T {
        self.visit_path(path)
    }
    fn visit_func_ty(&mut self, param_ty: &N<Ty>, return_ty: &N<Ty>) -> T;

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) -> T;
    fn visit_path(&mut self, path: &Path) -> T;
    fn visit_block(&mut self, block: &Block) -> T;
}
