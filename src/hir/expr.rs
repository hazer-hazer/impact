use crate::{
    parser::ast::expr::{InfixOp, Lit, PrefixOp},
    span::span::{Ident, Span, Spanned},
};

use super::stmt::Stmt;

pub type Expr<'a> = Spanned<ExprKind<'a>>;

pub enum ExprKind<'a> {
    Lit(Lit),
    Ident(Ident),
    Infix(&'a Expr<'a>, InfixOp, &'a Expr<'a>),
    Prefix(PrefixOp, &'a Expr<'a>),
    Abs(Ident, &'a Expr<'a>),
    App(&'a Expr<'a>, &'a Expr<'a>),
    Block(&'a [Stmt<'a>]),
    Let(Ident, &'a Expr<'a>, &'a Expr<'a>),
}
