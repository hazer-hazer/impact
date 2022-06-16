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

pub enum TyKind {
    Unit,
    Lit(LitTy),
    Var(PR<Ident>),
    Func(PR<N<Ty>>, PR<N<Ty>>),
    Paren(PR<N<Ty>>),
}

pub type Ty = Spanned<TyKind>;
