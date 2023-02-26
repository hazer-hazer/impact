use std::fmt::Display;

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

#[derive(Debug, Clone, Copy)]
pub enum BuiltinTy {
    Unit,
    I32,
}

impl Display for BuiltinTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BuiltinTy::Unit => "UnitTy",
                BuiltinTy::I32 => "I32",
            }
        )
    }
}

impl TryFrom<Builtin> for BuiltinTy {
    type Error = ();

    fn try_from(value: Builtin) -> Result<Self, Self::Error> {
        match value {
            Builtin::UnitTy => Ok(BuiltinTy::Unit),
            Builtin::I32 => Ok(BuiltinTy::I32),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct TyPath(pub Path);

#[derive(Debug)]
pub enum TyKind {
    Path(TyPath),
    Func(Ty, Ty),
    App(Ty, Ty),
    Builtin(BuiltinTy),
}

pub type Ty = HirId;
