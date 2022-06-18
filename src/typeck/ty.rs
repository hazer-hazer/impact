use std::fmt::Display;

use crate::{pp::PP, span::span::Ident};

#[derive(Clone)]
pub enum Ty {
    Var(Ident),
    Existential(Ident),
    Func(Box<Ty>, Box<Ty>),
    Forall(Ident, Box<Ty>),
}

pub struct TyError();
pub type TyResult<T> = Result<T, TyError>;

impl<'a> PP<'a> for Ty {
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        match self {
            Ty::Var(ident) => ident.ppfmt(sess),
            Ty::Existential(ident) => format!("{}^", ident.ppfmt(sess)),
            Ty::Func(param_ty, return_ty) => {
                format!("{} -> {}", param_ty.ppfmt(sess), return_ty.ppfmt(sess))
            }
            Ty::Forall(ident, ty) => format!("∀{}. {}", ident.ppfmt(sess), ty.ppfmt(sess)),
        }
    }
}

impl Ty {
    pub fn is_mono(&self) -> bool {
        match self {
            Ty::Var(_)
            | Ty::Existential(_) => true,
            Ty::Func(param_ty, return_ty) => param_ty.is_mono() && return_ty.is_mono(),
            Ty::Forall(_, _) => false,
        }
    }
}
