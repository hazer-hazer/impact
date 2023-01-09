use crate::span::span::{Ident, Span, WithSpan};

pub enum PatKind<'hir> {
    Ident(Ident),

    _RefStub(&'hir Pat<'hir>),
}

pub struct Pat<'hir> {
    kind: PatKind<'hir>,
    span: Span,
}

impl<'hir> Pat<'hir> {
    pub fn new(kind: PatKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &PatKind {
        &self.kind
    }
}

impl<'hir> WithSpan for Pat<'hir> {
    fn span(&self) -> Span {
        self.span
    }
}
