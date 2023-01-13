use crate::{
    ast::NodeId,
    span::span::{Span, WithSpan},
};

use super::{HirId, Path, N};

pub struct Ty {
    id: HirId,
    kind: TyKind,
    span: Span,
}

impl Ty {
    pub fn new(id: HirId, kind: TyKind, span: Span) -> Self {
        Self { id, kind, span }
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
