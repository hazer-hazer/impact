use super::{impl_with_hir_id, HirId, Pat, TyPath, WithHirId};
use crate::span::{impl_with_span, sym::Ident, Span, WithSpan};

#[derive(Debug)]
pub struct StructPatField {
    pub name: Option<Ident>,
    pub pat: Pat,
}

#[derive(Debug)]
pub enum PatKind {
    Unit,
    Ident(Ident, HirId),
    Struct(
        TyPath,
        Vec<StructPatField>,
        /// rest
        bool,
    ),
    Or(Pat, Pat),
    Tuple(Vec<Pat>),
}

#[derive(Debug)]
pub struct PatNode {
    id: HirId,
    kind: PatKind,
    span: Span,
}

impl_with_span!(PatNode);
impl_with_hir_id!(PatNode);

impl PatNode {
    pub fn new(id: HirId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &PatKind {
        &self.kind
    }
}
