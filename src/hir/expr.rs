use crate::{
    ast::expr::{InfixOp, Lit, PrefixOp},
    span::span::{Ident, Span, WithSpan},
};

use super::{stmt::Stmt, ty::Ty, N};

pub struct Expr {
    kind: ExprKind,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }
}

impl WithSpan for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Infix(N<Expr>, InfixOp, N<Expr>),
    Prefix(PrefixOp, N<Expr>),
    Abs(Ident, N<Expr>),
    App(N<Expr>, N<Expr>),
    Block(Vec<Stmt>),
    Let(Ident, N<Expr>, N<Expr>),
    Ty(N<Expr>, Ty),
}
