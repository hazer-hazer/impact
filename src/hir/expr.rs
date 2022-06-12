use crate::{
    ast::expr::{InfixOp, Lit, PrefixOp},
    span::span::{Ident, Spanned},
};

use super::{stmt::Stmt, N};

pub type Expr = Spanned<ExprKind>;

pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Infix(N<Expr>, InfixOp, N<Expr>),
    Prefix(PrefixOp, N<Expr>),
    Abs(Ident, N<Expr>),
    App(N<Expr>, N<Expr>),
    Block(Vec<Stmt>),
    Let(Ident, N<Expr>, N<Expr>),
}
