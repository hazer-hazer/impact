use crate::span::span::{Span, WithSpan};

use super::{expr::Expr, item::ItemId, HirId, WithHirId};

#[derive(Debug)]
pub struct StmtNode {
    id: HirId,
    pub kind: StmtKind,
    span: Span,
}

impl StmtNode {
    pub fn new(id: HirId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

impl WithHirId for StmtNode {
    fn id(&self) -> HirId {
        self.id
    }
}

impl WithSpan for StmtNode {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Expr),
    Item(ItemId),
}

pub type Stmt = HirId;
