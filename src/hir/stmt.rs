use crate::{
    span::span::{Span, WithSpan},
};

use super::{
    expr::{Expr},
    item::ItemId,
    HirId,
};

pub struct StmtNode {
    pub id: HirId,
    pub kind: StmtKind,
    span: Span,
}

impl StmtNode {
    pub fn new(id: HirId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

impl WithSpan for StmtNode {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum StmtKind {
    Expr(Expr),
    Item(ItemId),
}

pub type Stmt = HirId;
