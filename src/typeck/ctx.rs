use std::{collections::HashMap};

use crate::{
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, IndexVec},
    span::span::Ident,
};

use super::ty::Ty;

declare_idx!(ExistentialId, u32, "^{}", Color::Blue);

#[derive(Default, Clone)]
pub struct Ctx {
    existentials: Vec<ExistentialId>,
    // Note: I end up with storing `solved` and unsolved existentials separately due to `try` logic.
    //  We need to enter new "try to"-context under which we do not violate upper context.
    solved: IndexVec<ExistentialId, Option<Ty>>,
    vars: Vec<Ident>,
    terms: HashMap<Ident, Ty>,
}

impl Ctx {
    pub fn new_with_var(var: Ident) -> Self {
        Self {
            vars: vec![var],
            ..Default::default()
        }
    }

    pub fn new_with_ex(ex: ExistentialId) -> Self {
        Self {
            existentials: vec![ex],
            ..Default::default()
        }
    }

    pub fn new_with_term(name: Ident, ty: Ty) -> Self {
        Self {
            terms: HashMap::from([(name, ty)]),
            ..Default::default()
        }
    }

    // Getters //
    pub fn get_term(&self, name: Ident) -> Option<Ty> {
        self.terms.get(&name).copied()
    }

    pub fn has_var(&self, name: Ident) -> bool {
        self.get_var(name).is_some()
    }

    pub fn get_var(&self, name: Ident) -> Option<Ident> {
        self.vars.iter().find(|var| **var == name).copied()
    }

    pub fn has_ex(&self, ex: ExistentialId) -> bool {
        self.get_ex(ex).is_some()
    }

    pub fn get_ex(&self, ex: ExistentialId) -> Option<ExistentialId> {
        if let Some(_) = self.solved.get(ex) {
            Some(ex)
        } else {
            None
        }
    }

    pub fn get_solution(&self, ex: ExistentialId) -> Option<Ty> {
        self.solved.get_flat(ex).copied()
    }

    // Setters //
    pub fn add_var(&mut self, name: Ident) {
        assert!(!self.vars.contains(&name));
        self.vars.push(name);
    }

    pub fn add_ex(&mut self, ex: ExistentialId) {
        assert!(!self.existentials.contains(&ex));
        self.existentials.push(ex);
    }

    pub fn type_term(&mut self, name: Ident, ty: Ty) {
        assert!(self.terms.insert(name, ty).is_none())
    }

    /// Returns solution if existential is in context
    pub fn solve(&mut self, ex: ExistentialId, solution: Ty) -> Option<Ty> {
        if self.has_ex(ex) {
            assert!(self.solved[ex].replace(solution).is_none());
            Some(solution)
        } else {
            None
        }
    }
}
