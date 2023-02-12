use crate::span::span::{Span, WithSpan};

use super::{HirId, Path, WithHirId};

#[derive(Debug)]
pub struct TyNode {
    id: HirId,
    pub kind: TyKind,
    span: Span,
}

impl TyNode {
    pub fn new(id: HirId, kind: TyKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

impl WithHirId for TyNode {
    fn id(&self) -> HirId {
        self.id
    }
}

impl WithSpan for TyNode {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug)]
pub struct TyPath(pub Path);

#[derive(Debug)]
pub enum TyKind {
    Unit,
    Path(TyPath),
    Func(Ty, Ty),
}

pub type Ty = HirId;
