use std::fmt::Display;

use crate::{
    hir::N,
    span::span::{Ident, Span, WithSpan},
};

use super::{NodeId, PR};

#[derive(Clone, Copy)]
pub enum LitTy {
    Bool,
    Int,
    String,
}

impl Display for LitTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LitTy::Bool => "bool",
                LitTy::Int => "int",
                LitTy::String => "string",
            }
        )
    }
}

pub struct Ty {
    id: NodeId,
    kind: TyKind,
    span: Span,
}

impl Ty {
    pub fn new(id: NodeId, kind: TyKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }

    pub fn id(&self) -> NodeId {
        self.id
    }
}

impl WithSpan for Ty {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum TyKind {
    Unit,
    Lit(LitTy),
    Var(PR<Ident>),
    Func(PR<N<Ty>>, PR<N<Ty>>),
    Paren(PR<N<Ty>>),
}

// impl Display for TyKind {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             TyKind::Unit => write!(f, "()"),
//             TyKind::Lit(lit_ty) => write!(f, "{}", lit_ty),
//             TyKind::Var(ident) => write!(f, "{}", ident),
//             TyKind::Func(param_ty, return_ty) => write!(f, "{} -> {}", param_ty, return_ty),
//             TyKind::Paren(inner) => write!(f, "{}", inner),
//         }
//     }
// }
