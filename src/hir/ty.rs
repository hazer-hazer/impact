use crate::{
    span::span::{Span, WithSpan},
};

use super::{HirId, Path};

pub struct TyNode {
    pub id: HirId,
    pub kind: TyKind,
    span: Span,
}

impl TyNode {
    pub fn new(id: HirId, kind: TyKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

impl WithSpan for TyNode {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum TyKind {
    Unit,
    Path(Path),
    Func(Ty, Ty),
}

pub type Ty = HirId;
