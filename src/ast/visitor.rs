use crate::span::span::Ident;

use super::{
    expr::{Expr, ExprKind, Lit},
    stmt::Stmt,
    ErrorNode, AST, ty::{LitType, Type}, PR, N,
};

pub trait AstVisitor<T> {
    fn visit_err(&self, _: &ErrorNode) -> String {
        "[ERROR]".to_string()
    }

    fn visit_ast(&mut self, ast: &AST) -> T;
    fn visit_expr(&mut self, expr: &Expr) -> T;
    fn visit_lit_expr(&mut self, lit: &Lit) -> T;
    fn visit_ident_expr(&mut self, ident: &Ident) -> T;
    fn visit_infix_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_prefix_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_abs_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_app_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_block_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_let_expr(&mut self, expr: &ExprKind) -> T;
    fn visit_type_expr(&mut self, expr: &ExprKind) -> T;

    fn visit_stmt(&mut self, stmt: &Stmt) -> T;

    fn visit_ty(&mut self, ty: &Type) -> T;
    fn visit_unit_ty(&mut self) -> T;
    fn visit_lit_ty(&mut self, lit_ty: &LitType) -> T;
    fn visit_var_ty(&mut self, ident: &Ident) -> T;
    fn visit_func_ty(&mut self, param_ty: &PR<N<Type>>, return_ty: &PR<N<Type>>) -> T;

    fn visit_ident(&mut self, ident: &Ident) -> T;
}
