use crate::{
    ast::{
        expr::{InfixOp, Lit, PrefixOp},
        NodeId,
    },
    span::span::{Ident, Span, WithSpan},
};

use super::{stmt::Stmt, ty::Ty, Path, N};

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

pub struct Block {
    stmts: Vec<Stmt>,
    expr: N<Expr>,
}

impl Block {
    pub fn new(stmts: Vec<Stmt>, expr: N<Expr>) -> Self {
        Self { stmts, expr }
    }

    pub fn stmts(&self) -> &[Stmt] {
        self.stmts.as_ref()
    }

    pub fn expr(&self) -> &Expr {
        self.expr.as_ref()
    }
}

pub enum ExprKind {
    Lit(Lit),
    Path(Path),
    Infix(N<Expr>, InfixOp, N<Expr>),
    Prefix(PrefixOp, N<Expr>),
    Abs(Ident, N<Expr>),
    App(N<Expr>, N<Expr>),
    Let(Block),
    Ty(N<Expr>, Ty),
}
