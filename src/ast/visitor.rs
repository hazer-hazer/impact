use crate::span::span::Ident;

use super::{
    expr::{Expr, Lit, InfixOp, PrefixOp},
    stmt::Stmt,
    ErrorNode, AST, ty::{LitTy, Ty}, PR, N,
};

pub trait AstVisitor<T> {
    fn visit_err(&self, _: &ErrorNode) -> String {
        "[ERROR]".to_string()
    }

    fn visit_ast(&mut self, ast: &AST) -> T;
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) -> T;
    fn visit_lit_expr(&mut self, lit: &Lit) -> T;
    fn visit_ident_expr(&mut self, ident: &Ident) -> T;
    fn visit_infix_expr(&mut self, lhs: &PR<N<Expr>>, op: &InfixOp, rhs: &PR<N<Expr>>) -> T;
    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) -> T;
    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) -> T;
    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) -> T;
    fn visit_block_expr(&mut self, stmts: &Vec<PR<N<Stmt>>>) -> T;
    fn visit_let_expr(&mut self, name: &PR<Ident>, value: &PR<N<Expr>>, body: &PR<N<Expr>>) -> T;
    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) -> T;

    // Types //
    fn visit_ty(&mut self, ty: &Ty) -> T;
    fn visit_unit_ty(&mut self) -> T;
    fn visit_lit_ty(&mut self, lit_ty: &LitTy) -> T;
    fn visit_var_ty(&mut self, ident: &PR<Ident>) -> T;
    fn visit_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) -> T;
    fn visit_paren_ty(&mut self, inner: &PR<N<Ty>>) -> T;

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) -> T;
}
