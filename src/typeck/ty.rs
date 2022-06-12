use std::collections::HashMap;

use crate::{session::Session, span::span::Symbol};

#[derive(Debug, Clone)]
pub enum LitTy {
    Bool,
    Int,
    String,
}

#[derive(Debug, Clone)]
pub enum Type {
    Lit(LitTy),
    Func(Box<Type>, Box<Type>),
    Forall(Symbol, Box<Type>),
}

#[derive(Clone)]
pub enum CtxEl {
    Var(Symbol),
    TypedVar(Symbol, Type),
    Exist(Symbol),
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

    pub fn extend(&self, el: CtxEl) -> TyCtx {
        let mut els = self.els.clone();
        els.push(el);
        Self { els }
    }
}

#[derive(Clone, Debug)]
pub struct State {
    existentials: usize,
}

impl State {
    fn initial() -> Self {
        Self { existentials: 0 }
    }

    fn fresh(&mut self, sess: &mut Session) -> Symbol {
        let sym = sess.intern(format!("{}^", self.existentials));
        self.existentials += 1;
        sym
    }
}
