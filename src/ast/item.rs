use crate::span::span::{Ident, Span, WithSpan};

use super::{ty::Ty, NodeId, N, PR};

pub enum ItemKind {
    Type(PR<Ident>, PR<N<Ty>>),
}

pub struct Item {
    id: NodeId,
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(id: NodeId, kind: ItemKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }
}

impl WithSpan for Item {
    fn span(&self) -> Span {
        self.span
    }
}
