use std::fmt::Display;

use crate::{span::span::Ident};

#[derive(Clone)]
pub enum Ty {
    Var(Ident),
    Existential(Ident),
    Func(Box<Ty>, Box<Ty>),
    Forall(Ident, Box<Ty>),
}

pub struct TyError();
pub type TyResult<T> = Result<T, TyError>;

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Ty::Var(ident) => ident,
            Ty::Existential(ident) => format!("{}^", ident),
            Ty::Func(param_ty, return_ty) => {
                format!("{} -> {}", param_ty, return_ty)
            }
            Ty::Forall(ident, ty) => format!("∀{}. {}", ident, ty),
        })
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
