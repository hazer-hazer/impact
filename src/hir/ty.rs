use crate::span::span::{Span, WithSpan};

use super::Path;

pub struct Ty<'hir> {
    kind: TyKind<'hir>,
    span: Span,
}

impl<'hir> Ty<'hir> {
    pub fn new(kind: TyKind<'hir>, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }
}

impl<'hir> WithSpan for Ty<'hir> {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum TyKind<'hir> {
    Unit,
    Path(&'hir Path<'hir>),
    Func(&'hir Ty<'hir>, &'hir Ty<'hir>),
}
