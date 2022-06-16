use crate::{span::span::{Spanned, Ident}, hir::N, pp::PP, session::Session};

use super::PR;

pub enum LitTy {
    Bool,
    Int,
    String,
}

pub enum TyKind {
    Unit,
    Lit(LitTy),
    Var(PR<Ident>),
    Func(PR<N<Ty>>, PR<N<Ty>>),
    Paren(PR<N<Ty>>),
}

pub type Ty = Spanned<TyKind>;
