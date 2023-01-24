use crate::span::span::{Ident, Span, WithSpan};

use super::HirId;

pub enum PatKind {
    Ident(Ident),
}

pub struct PatNode {
    pub id: HirId,
    pub kind: PatKind,
    span: Span,
}

impl PatNode {
    pub fn new(id: HirId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

impl WithSpan for PatNode {
    fn span(&self) -> Span {
        self.span
    }
}

pub type Pat = HirId;
