use crate::{span::span::Ident};

use super::{expr::{Expr, Lit, ExprKind}, stmt::{Stmt, LetStmt}, AST};

pub trait Visitor<T> {
    fn visit_ast(&mut self, ast: &AST) -> T;
    fn visit_expr(&mut self, expr: &Expr) -> T;
    fn visit_lit_expr(&mut self, lit: &Lit) -> T;
    fn visit_ident_expr(&mut self, ident: &Ident) -> T;
    fn visit_infix_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_prefix_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_app_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_block_expr(&mut self, expr: &ExprKind) -> T;

    fn visit_stmt(&mut self, stmt: &Stmt) -> T;
    fn visit_let_stmt(&mut self, stmt: &LetStmt) -> T;

    fn visit_ident(&mut self, ident: &Ident) -> T;
}
