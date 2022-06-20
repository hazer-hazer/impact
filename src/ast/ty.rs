use std::fmt::Display;

use crate::{hir::N, span::span::Ident};

use super::PR;

use crate::span::span::Spanned;

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

pub type Ty = Spanned<TyKind>;

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
