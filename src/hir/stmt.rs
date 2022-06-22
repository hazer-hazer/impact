use crate::span::span::{Span, Spanned, WithSpan};

use super::{expr::Expr, item::Item};

pub struct Stmt {
    kind: StmtKind,
    span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }
}

impl WithSpan for Stmt {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum StmtKind {
    Expr(Expr),
    Item(Item),
}
