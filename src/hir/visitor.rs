use crate::{ast::{expr::Lit, ty::LitTy}, span::span::Ident};

use super::{
    expr::{Expr, ExprKind},
    stmt::Stmt,
    HIR, ty::Ty, N,
};

pub trait HirVisitor<T> {
    fn visit_hir(&mut self, hir: &HIR) -> T;
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) -> T;
    fn visit_lit_expr(&mut self, lit: &Lit) -> T;
    fn visit_ident_expr(&mut self, ident: &Ident) -> T;
    fn visit_infix_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_prefix_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_abs_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_app_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_block_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_let_expr(&mut self, expr: &ExprKind) -> T;

    // Types //
    fn visit_ty(&mut self, ty: &Ty) -> T;
    fn visit_unit_ty(&mut self) -> T;
    fn visit_lit_ty(&mut self, lit_ty: &LitTy) -> T;
    fn visit_var_ty(&mut self, ident: &Ident) -> T;
    fn visit_func_ty(&mut self, param_ty: &N<Ty>, return_ty: &N<Ty>) -> T;

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) -> T;
}
