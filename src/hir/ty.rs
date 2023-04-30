use super::{HirId, Ty, TyPath, WithHirId, impl_with_hir_id};
use crate::{
    resolve::builtin::TyBuiltin,
    span::{impl_with_span, Span, WithSpan},
};

#[derive(Debug)]
pub struct TyNode {
    id: HirId,
    kind: TyKind,
    span: Span,
}

impl_with_span!(TyNode);
impl_with_hir_id!(TyNode);

impl TyNode {
    pub fn new(id: HirId, kind: TyKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }
}


#[derive(Debug)]
pub enum TyKind {
    Path(TyPath),
    Func(Vec<Ty>, Ty),
    App(Ty, Vec<Ty>),
    Builtin(TyBuiltin),
}
