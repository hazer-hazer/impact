use crate::span::span::{Ident, Span, WithSpan, impl_with_span};

use super::{HirId, WithHirId};

#[derive(Debug)]
pub enum PatKind {
    Unit,
    Ident(Ident),
}

#[derive(Debug)]
pub struct PatNode {
    id: HirId,
    kind: PatKind,
    span: Span,
}

impl PatNode {
    pub fn new(id: HirId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &PatKind {
        &self.kind
    }
}

impl WithHirId for PatNode {
    fn id(&self) -> HirId {
        self.id
    }
}

impl_with_span!(PatNode);

pub type Pat = HirId;
