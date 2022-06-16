use std::fmt::Display;

use crate::{pp::PP, span::span::Ident};

#[derive(Clone)]
pub enum Ty {
    Var(Ident),
    Existential(Ident),
    Func(Box<Ty>, Box<Ty>),
    Forall(Ident, Box<Ty>),
}

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
