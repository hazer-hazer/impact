use std::fmt::Display;

use crate::{
    hir::N,
    span::span::{Ident, Spanned},
};

use super::PR;

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
    Var(Ident),
    Func(PR<N<Ty>>, PR<N<Ty>>),
}

pub type Ty = Spanned<TyKind>;
