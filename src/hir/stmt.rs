use crate::span::span::{Span, WithSpan};

use super::{expr::Expr, item::Item};

pub struct Stmt<'hir> {
    kind: StmtKind<'hir>,
    span: Span,
}

impl<'hir> Stmt<'hir> {
    pub fn new(kind: StmtKind<'hir>, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }
}

impl<'hir> WithSpan for Stmt<'hir> {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum StmtKind<'hir> {
    Expr(&'hir Expr<'hir>),
    Item(&'hir Item<'hir>),
}
