use crate::{
    ast::NodeId,
    span::span::{Ident, Span, WithSpan},
};

use super::HirId;

pub enum PatKind {
    Ident(Ident),
}

pub struct Pat {
    id: HirId,
    kind: PatKind,
    span: Span,
}

impl Pat {
    pub fn new(id: HirId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &PatKind {
        &self.kind
    }
}

impl WithSpan for Pat {
    fn span(&self) -> Span {
        self.span
    }
}
