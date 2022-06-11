use crate::{ast::expr::Lit, span::span::Ident};

use super::{HIR, expr::{Expr, ExprKind}, stmt::Stmt};

pub trait Visitor<'a, T> {
    fn visit_hir(&mut self, hir: &'a HIR) -> T;
    fn visit_expr(&mut self, expr: &'a Expr) -> T;
    fn visit_lit_expr(&mut self, lit: &Lit) -> T;
    fn visit_ident_expr(&mut self, ident: &Ident) -> T;
    fn visit_infix_expr(&mut self, expr: &'a ExprKind<'a>) -> T;
    fn visit_prefix_expr(&mut self, expr: &'a ExprKind<'a>) -> T;
    fn visit_abs_expr(&mut self, expr: &'a ExprKind<'a>) -> T;
    fn visit_app_expr(&mut self, expr: &'a ExprKind<'a>) -> T;
    fn visit_block_expr(&mut self, expr: &'a ExprKind<'a>) -> T;
    fn visit_let_expr(&mut self, expr: &'a ExprKind<'a>) -> T;

    fn visit_stmt(&mut self, stmt: &'a Stmt<'a>) -> T;

    fn visit_ident(&mut self, ident: &Ident) -> T;
}
