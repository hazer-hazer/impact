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

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TyKind::Unit => todo!(),
                TyKind::Lit(_) => todo!(),
                TyKind::Var(_) => todo!(),
                TyKind::Func(_, _) => todo!(),
                TyKind::Paren(_) => todo!(),
            }
        )
    }
}
