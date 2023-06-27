use super::{HirId, WithHirId, impl_with_hir_id};
use crate::span::{impl_with_span, sym::Ident, Span, WithSpan};

#[derive(Debug)]
pub enum PatKind {
    Unit,
    Ident(Ident, HirId),
}

#[derive(Debug)]
pub struct PatNode {
    id: HirId,
    kind: PatKind,
    span: Span,
}

impl_with_span!(PatNode);
impl_with_hir_id!(PatNode);

impl PatNode {
    pub fn new(id: HirId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &PatKind {
        &self.kind
    }
}

