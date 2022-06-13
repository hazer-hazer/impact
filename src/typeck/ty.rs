use std::collections::HashMap;

use crate::{session::Session, span::span::Symbol};

#[derive(Debug, Clone, PartialEq)]
pub enum LitTy {
    Bool,
    Int,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Lit(LitTy),
    Var(Symbol),
    Func(Box<Type>, Box<Type>),
    Forall(Symbol, Box<Type>),
    Existential(Symbol),
}

impl Type {
    fn apply_ctx(self, ctx: &TyCtx) -> Self {
        match self {
            Type::Lit(_) => self,
            Type::Var(_) => self,
            Type::Func(param, body) => Type::Func(
                Box::new(param.apply_ctx(ctx)),
                Box::new(body.apply_ctx(ctx)),
            ),
            Type::Forall(name, ty) => Type::Forall(name, Box::new(ty.apply_ctx(ctx))),
            Type::Existential(name) => {
                if let Some(tau) = ctx.get_solved(name) {
                    tau.clone().apply_ctx(ctx)
                } else {
                    self
                }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum CtxEl {
    Var(Symbol),
    TypedVar(Symbol, Type),
    Exist(Symbol),
    Solved(Symbol, Type),
}

#[derive(Default, Clone)]
pub struct TyCtx {
    els: Vec<CtxEl>,
}

impl TyCtx {
    pub fn initial() -> Self {
        Self {
            els: Default::default(),
        }
    }

    pub fn new(els: Vec<CtxEl>) -> Self {
        Self { els }
    }

    pub fn extended(&self, el: CtxEl) -> TyCtx {
        let mut els = self.els.clone();
        els.push(el);
        Self { els }
    }

    pub fn drop(&self, to_drop: CtxEl) -> Self {
        if let Some(index) = self.els.iter().position(|el| el == &to_drop) {
            let mut els = self.els.clone();
            els.split_off(index);
            Self { els }
        } else {
            unreachable!()
        }
    }

    pub fn get_solved(&self, name: Symbol) -> Option<&Type> {
        for el in &self.els {
            if let CtxEl::Solved(solved_name, tau) = el {
                if name == *solved_name {
                    return Some(tau);
                }
            }
        }
        None
    }

    pub fn add(&self, el: CtxEl, inserts: Vec<CtxEl>) -> Self {
        if let Some(index) = self.els.iter().position(|el| el == &el)
    }
}

#[derive(Clone, Debug)]
pub struct State {
    existentials: usize,
}

impl State {
    pub fn initial() -> Self {
        Self { existentials: 0 }
    }

    pub fn fresh(&mut self, sess: &mut Session) -> Symbol {
        let sym = sess.intern(format!("{}^", self.existentials));
        self.existentials += 1;
        sym
    }
}
