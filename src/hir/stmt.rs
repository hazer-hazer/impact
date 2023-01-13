use crate::{
    ast::NodeId,
    span::span::{Span, WithSpan},
};

use super::{expr::Expr, item::ItemId, HirId, N};

pub struct Stmt {
    id: HirId,
    kind: StmtKind,
    span: Span,
}

impl Stmt {
    pub fn new(id: HirId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
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
    Expr(N<Expr>),
    Item(ItemId),
}
