use super::{item::ItemId, Expr, HirId, WithHirId};
use crate::span::{impl_with_span, sym::Ident, Span, WithSpan};

#[derive(Debug)]
pub struct StmtNode {
    id: HirId,
    kind: StmtKind,
    span: Span,
}

impl StmtNode {
    pub fn new(id: HirId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }
}

impl WithHirId for StmtNode {
    fn id(&self) -> HirId {
        self.id
    }
}

impl_with_span!(StmtNode);

#[derive(Debug)]
pub struct Local {
    pub id: HirId,
    pub name: Ident,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Local(Local),
    Expr(Expr),
    Item(ItemId),
}
