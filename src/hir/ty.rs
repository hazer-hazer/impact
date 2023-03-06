

use crate::{
    resolve::builtin::Builtin,
    span::span::{Span, WithSpan},
};

use super::{HirId, Path, WithHirId};

#[derive(Debug)]
pub struct TyNode {
    id: HirId,
    kind: TyKind,
    span: Span,
}

impl TyNode {
    pub fn new(id: HirId, kind: TyKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
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
    Path(TyPath),
    Func(Ty, Ty),
    App(Ty, Ty),
    Builtin(Builtin),
}

pub type Ty = HirId;
