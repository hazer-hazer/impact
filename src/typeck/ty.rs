use std::{collections::HashMap, fmt::Display};

use crate::span::span::Ident;

use super::ctx::{Ctx, CtxItem, CtxItemName};

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

#[derive(Clone)]
pub enum TyKind {
    Unit,
    Lit(LitTy),
    Var(Ident),
    Existential(Ident),
    Func(Box<Ty>, Box<Ty>),
    Forall(Ident, Box<Ty>),
}

#[derive(Clone)]
pub struct Ty {
    kind: TyKind,
}

impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Self { kind }
    }

    pub fn lit(lit_ty: LitTy) -> Self {
        Self::new(TyKind::Lit(lit_ty))
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }
}

pub struct TyError();
pub type TyResult<T> = Result<T, TyError>;

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            TyKind::Unit => write!(f, "()"),
            TyKind::Lit(lit) => write!(f, "{}", lit),
            TyKind::Var(ident) => write!(f, "{}", ident),
            TyKind::Existential(ident) => write!(f, "{}^", ident),
            TyKind::Func(param_ty, return_ty) => write!(f, "{} -> {}", param_ty, return_ty),
            TyKind::Forall(ident, ty) => write!(f, "∀{}. {}", ident, ty),
        }
    }
}

impl Ty {
    pub fn is_mono(&self) -> bool {
        match self.kind() {
            TyKind::Unit | TyKind::Lit(_) | TyKind::Var(_) | TyKind::Existential(_) => true,
            TyKind::Func(param_ty, return_ty) => param_ty.is_mono() && return_ty.is_mono(),
            TyKind::Forall(_, _) => false,
        }
    }

    /// Substitute
    pub fn substitute(&self, name: Ident, with: Ty) -> Ty {
        match self.kind() {
            TyKind::Unit | TyKind::Lit(_) => self.clone(),
            TyKind::Var(ident) => {
                if name == *ident {
                    with.clone()
                } else {
                    self.clone()
                }
            }
            TyKind::Existential(_) => self.clone(),
            TyKind::Func(param_ty, return_ty) => Ty::new(TyKind::Func(
                Box::new(param_ty.substitute(name, with.clone())),
                Box::new(return_ty.substitute(name, with.clone())),
            )),
            TyKind::Forall(ident, ty) => Ty::new(if name == *ident {
                TyKind::Forall(*ident, Box::new(with.clone()))
            } else {
                TyKind::Forall(*ident, Box::new(ty.substitute(name, with)))
            }),
        }
    }

    /// Substitute all occurrences of universally quantified type inside it body
    pub fn open_forall(&self, subst: Ty) -> Ty {
        match self.kind() {
            TyKind::Forall(ident, ty) => self.substitute(*ident, *ty.clone()),
            _ => unreachable!(),
        }
    }

    pub fn substitute_existentials(&self, existentials: &HashMap<Ident, Ty>) -> Ty {
        match self.kind() {
            TyKind::Unit | TyKind::Lit(_) | TyKind::Var(_) => self.clone(),
            TyKind::Existential(ident) => match existentials.get(ident) {
                Some(ty) => ty.clone(),
                None => self.clone(),
            },
            TyKind::Func(param_ty, return_ty) => Ty::new(TyKind::Func(
                Box::new(param_ty.substitute_existentials(existentials)),
                Box::new(return_ty.substitute_existentials(existentials)),
            )),
            TyKind::Forall(ident, ty) => Ty::new(TyKind::Forall(
                *ident,
                Box::new(ty.substitute_existentials(existentials)),
            )),
        }
    }

    pub fn apply_ctx(&self, ctx: &Ctx) -> Ty {
        match self.kind() {
            TyKind::Unit | TyKind::Lit(_) | TyKind::Var(_) => self.clone(),
            TyKind::Existential(ident) => match ctx.lookup(CtxItemName::Existential(*ident)) {
                Some(item) => match item {
                    CtxItem::TypedTerm(_, ty) => ty.clone(),
                    CtxItem::Existential(_, ty) => {
                        if let Some(ty) = ty {
                            ty.clone()
                        } else {
                            self.clone()
                        }
                    }
                    _ => self.clone(),
                },
                None => self.clone(),
            },
            TyKind::Func(param_ty, return_ty) => Ty::new(TyKind::Func(
                Box::new(param_ty.apply_ctx(ctx)),
                Box::new(return_ty.apply_ctx(ctx)),
            )),
            TyKind::Forall(ident, ty) => {
                Ty::new(TyKind::Forall(*ident, Box::new(ty.apply_ctx(ctx))))
            }
        }
    }
}
