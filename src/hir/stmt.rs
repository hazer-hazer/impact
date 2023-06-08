use super::{impl_with_hir_id, item::ItemId, Expr, HirId, Pat, WithHirId};
use crate::span::{impl_with_span, Span, WithSpan};

#[derive(Debug)]
pub struct StmtNode {
    id: HirId,
    kind: StmtKind,
    span: Span,
}

impl_with_span!(StmtNode);
impl_with_hir_id!(StmtNode);

impl StmtNode {
    pub fn new(id: HirId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }
}

#[derive(Debug)]
pub struct Local {
    pub id: HirId,
    pub pat: Pat,
    pub value: Expr,
    pub span: Span,
}

impl_with_hir_id!(Local);

#[derive(Debug)]
pub enum StmtKind {
    Local(Local),
    Expr(Expr),
    Item(ItemId),
}
