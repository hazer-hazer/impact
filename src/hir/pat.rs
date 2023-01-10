use crate::span::span::{Ident, Span, WithSpan};

pub enum PatKind {
    Ident(Ident),
}

pub struct Pat {
    kind: PatKind,
    span: Span,
}

impl Pat {
    pub fn new(kind: PatKind, span: Span) -> Self {
        Self { kind, span }
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
