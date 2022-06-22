use crate::span::span::{Ident, Span, WithSpan};

use super::{ty::Ty, N};

pub enum ItemKind {
    Type(Ident, N<Ty>),
}

pub struct Item {
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(kind: ItemKind, span: Span) -> Self {
        Self { kind, span }
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
