use crate::span::span::{Span, WithSpan};

use super::{Path, N};

pub struct Ty {
    kind: TyKind,
    span: Span,
}

impl Ty {
    pub fn new(kind: TyKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }
}

impl WithSpan for Ty {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum TyKind {
    Unit,
    Path(Path),
    Func(N<Ty>, N<Ty>),
}
