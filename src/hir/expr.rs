use crate::{
    ast::expr::{InfixOp, Lit, PrefixOp},
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
    expr: Option<N<Expr>>,
}

impl Block {
    pub fn new(stmts: Vec<Stmt>, expr: Option<N<Expr>>) -> Self {
        Self { stmts, expr }
    }

    pub fn stmts(&self) -> &[Stmt] {
        self.stmts.as_ref()
    }

    pub fn expr(&self) -> Option<&N<Expr>> {
        self.expr.as_ref()
    }
}

pub enum ExprKind {
    Unit,
    Lit(Lit),
    Path(Path),
    Block(Block),
    Infix(N<Expr>, InfixOp, N<Expr>),
    Prefix(PrefixOp, N<Expr>),
    Abs(Ident, N<Expr>),
    App(N<Expr>, N<Expr>),
    Let(Block),
    Ty(N<Expr>, Ty),
}
