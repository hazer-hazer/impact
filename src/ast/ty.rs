use std::fmt::Display;

use crate::{
    hir::N,
    span::span::{Ident, Spanned},
};

use super::PR;

#[derive(Clone, Copy)]
pub enum LitType {
    Bool,
    Int,
    String,
}

impl Display for LitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LitType::Bool => "bool",
                LitType::Int => "int",
                LitType::String => "string",
            }
        )
    }
}

pub enum TypeKind {
    Unit,
    Lit(LitType),
    Var(Ident),
    Func(PR<N<Type>>, PR<N<Type>>),
}

pub type Type = Spanned<TypeKind>;
