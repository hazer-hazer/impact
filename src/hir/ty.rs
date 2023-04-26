use super::{HirId, Ty, TyPath, WithHirId};
use crate::{
    resolve::builtin::{TyBuiltin},
    span::{impl_with_span, Span, WithSpan},
};

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

impl_with_span!(TyNode);

#[derive(Debug)]
pub enum TyKind {
    Path(TyPath),
    Func(Vec<Ty>, Ty),
    App(Ty, Vec<Ty>),
    Builtin(TyBuiltin),
}
