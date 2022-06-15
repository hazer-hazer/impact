use crate::{
    ast::ty::LitTy,
    span::span::{Ident, Spanned},
};

use super::N;

pub enum TyKind {
    Unit,
    Lit(LitTy),
    Var(Ident),
    Func(N<Ty>, N<Ty>),
}

pub type Ty = Spanned<TyKind>;
