use std::{collections::HashMap, fmt::Display};

use crate::{
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, IndexVec},
    span::span::{Ident, Symbol},
};

use super::ty::{Existential, Ty, ExistentialId, ExistentialKind};

#[derive(Default, Clone, Debug)]
pub struct InferCtx {
    existentials: Vec<Existential>,

    // Note: I end up with storing `solved` and unsolved existentials separately due to `try` logic.
    //  We need to enter new "try to"-context under which we do not violate upper context.
    solved: IndexVec<ExistentialId, Option<Ty>>,
    vars: Vec<Symbol>,
    terms: HashMap<Symbol, Ty>,
}

impl InferCtx {
    pub fn new_with_var(var: Ident) -> Self {
        Self {
            vars: vec![var.sym()],
            ..Default::default()
        }
    }

    pub fn new_with_ex(ex: Existential) -> Self {
        Self {
            existentials: vec![ex],
            ..Default::default()
        }
    }

    pub fn new_with_term(name: Ident, ty: Ty) -> Self {
        Self {
            terms: HashMap::from([(name.sym(), ty)]),
            ..Default::default()
        }
    }

    // Getters //
    pub fn get_term(&self, name: Ident) -> Option<Ty> {
        self.terms.get(&name.sym()).copied()
    }

    pub fn has_var(&self, name: Ident) -> bool {
        self.get_var(name).is_some()
    }

    pub fn get_var(&self, name: Ident) -> Option<Symbol> {
        self.vars.iter().find(|&&var| var == name.sym()).copied()
    }

    pub fn has_ex(&self, ex: Existential) -> bool {
        self.existentials.iter().any(|&ex_| ex == ex_)
    }

    /// Seems to be a strange function, but useful in `ascend_ctx` checks
    pub fn get_ex(&self, ex: Existential) -> Option<Existential> {
        if self.has_ex(ex) {
            Some(ex)
        } else {
            None
        }
    }

    pub fn get_solution(&self, ex: Existential) -> Option<Ty> {
        self.solved.get_flat(ex.id()).copied()
    }

    // pub fn get_unsolved(&self) -> Vec<ExistentialId> {
    //     self.existentials
    //         .clone()
    //         .into_iter()
    //         .filter(|ex| self.get_solution(ex.id()).is_some())
    //         .collect::<Vec<_>>()
    // }

    // Setters //
    pub fn add_var(&mut self, name: Ident) {
        assert!(!self.vars.contains(&name.sym()));
        self.vars.push(name.sym());
    }

    pub fn add_ex(&mut self, ex: Existential) {
        assert!(!self.has_ex(ex));
        self.existentials.push(ex);
    }

    pub fn type_term(&mut self, name: Ident, ty: Ty) {
        assert!(
            self.terms.insert(name.sym(), ty).is_none(),
            "{} is already typed as {}",
            name,
            ty,
        )
    }

    /// Returns solution if existential is in context
    pub fn solve(&mut self, ex: Existential, solution: Ty) -> Option<Ty> {
        if self.has_ex(ex) {
            assert!(self.solved.insert(ex.id(), solution).is_none());
            Some(solution)
        } else {
            None
        }
    }

    pub fn existentials(&self) -> &[Existential] {
        self.existentials.as_ref()
    }

    pub fn int_exes(&self) -> Vec<Existential> {
        self.existentials()
            .iter()
            .filter_map(|&ex| match ex.kind() {
                ExistentialKind::Int => Some(ex),
                _ => None,
            })
            .collect()
    }

    pub fn float_exes(&self) -> Vec<Existential> {
        self.existentials()
            .iter()
            .filter_map(|&ex| match ex.kind() {
                ExistentialKind::Float => Some(ex),
                _ => None,
            })
            .collect()
    }
}
