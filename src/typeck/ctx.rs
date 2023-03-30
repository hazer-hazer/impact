use std::collections::HashMap;

use crate::{
    cli::verbose,
    dt::idx::IndexVec,
    span::span::{Ident, Symbol},
};

use super::ty::{Existential, ExistentialId, ExistentialKind, Ty, TyKind, TyVarId};

#[derive(Default, Debug)]
pub struct GlobalCtx {
    solved: IndexVec<ExistentialId, Option<Ty>>,

    // FIXME: This might be absolutely wrong
    existentials: IndexVec<ExistentialId, Option<(usize, usize)>>,
}

impl GlobalCtx {
    /**
     * Add context, mostly after popping it from stack.
     * When we exit context, all existentials need to be saved
     * because we need them to get a solution for solved types
     * having either successfully inferred type or a typeck error.
     */
    pub fn add(&mut self, depth: usize, ctx: InferCtx) {
        ctx.existentials()
            .iter()
            .enumerate()
            .for_each(|(index, &ex)| {
                if let Some(sol) = ctx.get_solution(ex) {
                    verbose!("Add ex to global ctx {} = {}", ex, sol);
                    assert!(self.solved.insert(ex.id(), sol).is_none());
                } else {
                    verbose!("Add ex to global ctx {}", ex);
                    assert!(self.existentials.insert(ex.id(), (depth, index)).is_none())
                }
            });
    }

    pub fn get_solution(&self, ex: Existential) -> Option<Ty> {
        self.solved.get_flat(ex.id()).copied()
    }

    pub fn has_ex(&self, ex: Existential) -> bool {
        self.existentials().contains(&ex.id()) || self.solved.has(ex.id())
    }

    pub fn get_ex_index(&self, ex: Existential) -> Option<(usize, usize)> {
        self.existentials.get_flat(ex.id()).copied()
    }

    pub fn solved(&self) -> &IndexVec<ExistentialId, Option<Ty>> {
        &self.solved
    }

    // FIXME: Copypaste
    pub fn solve(&mut self, ex: Existential, sol: Ty) -> Option<Ty> {
        if self.has_ex(ex) {
            assert!(
                self.solved.insert(ex.id(), sol).is_none(),
                "Tried to override solution of {} = {} to {}",
                ex,
                self.solved.get_unwrap(ex.id()),
                sol
            );
            Some(sol)
        } else {
            None
        }
    }

    /**
     * Applies global context on the type.
     * If returned type contains unsolved existentials -- we failed to infer its type.
     */
    pub fn apply_on(&self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Var(_) => ty,
            // FIXME: Can we panic on unwrap?
            &TyKind::Existential(ex) => self.apply_on(
                self.get_solution(ex)
                    .expect(&format!("Unsolved existential {}", ex)),
            ),
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => Ty::func(
                ty.func_def_id(),
                params.iter().map(|param| self.apply_on(*param)).collect(),
                self.apply_on(*body),
            ),
            &TyKind::Forall(alpha, body) => Ty::forall(alpha, self.apply_on(body)),
            &TyKind::Ref(inner) => Ty::ref_to(self.apply_on(inner)),
        }
    }

    pub fn existentials(&self) -> Vec<ExistentialId> {
        self.existentials
            .iter_enumerated_flat()
            .map(|(ex, _)| ex)
            .collect()
    }

    pub fn unsolved(&self) -> Vec<ExistentialId> {
        self.existentials
            .iter_enumerated_flat()
            .filter_map(|(ex, _)| {
                if let None = self.solved.get(ex) {
                    Some(ex)
                } else {
                    None
                }
            })
            .collect()
    }
}

/**
 * InferCtx is a common abstraction similar to algorithmic context
 * from CAEBTC.
 */
#[derive(Default, Clone, Debug)]
pub struct InferCtx {
    /// Existentials defined in current context
    existentials: Vec<Existential>,

    /// Existentials solved in current context. Leak to global context.
    // Note: I end up with storing `solved` and unsolved existentials separately due to `try` logic.
    //  We need to enter new "try to"-context under which we do not violate upper context.
    solved: IndexVec<ExistentialId, Option<Ty>>,

    // FIXME: Unused?
    func_param_solutions: IndexVec<ExistentialId, Option<Vec<Ty>>>,

    /// Type variables defined in current context. Do not leak to global context.
    /// Actually only used for well-formedness.
    vars: Vec<TyVarId>,

    /// Typed terms. Do not leak to global context
    /// FIXME: Possibly get rid of this. Could be replaced with `HirId -> Ty` bindings globally.
    terms: HashMap<Symbol, Ty>,
}

impl InferCtx {
    // Constructors //
    pub fn new_with_var(var: TyVarId) -> Self {
        Self {
            vars: Vec::from([var]),
            ..Default::default()
        }
    }

    pub fn new_with_ex(ex: Existential) -> Self {
        Self {
            existentials: Vec::from([ex]),
            ..Default::default()
        }
    }

    pub fn new_with_term(name: Ident, ty: Ty) -> Self {
        Self {
            terms: HashMap::from([(name.sym(), ty)]),
            ..Default::default()
        }
    }

    pub fn new_with_term_map(names: &[Ident], tys: &[Ty]) -> Self {
        assert_eq!(names.len(), tys.len());
        Self {
            terms: names
                .iter()
                .map(|name| name.sym())
                .zip(tys.iter().copied())
                .collect::<HashMap<_, _>>(),
            ..Default::default()
        }
    }

    // Getters //
    pub fn get_term(&self, name: Ident) -> Option<Ty> {
        self.terms.get(&name.sym()).copied()
    }

    pub fn get_var(&self, var: TyVarId) -> Option<TyVarId> {
        self.vars.iter().find(|&&_var| var == _var).copied()
    }

    pub fn get_ex_index(&self, ex: Existential) -> Option<usize> {
        self.existentials.iter().position(|&ex_| ex == ex_)
    }

    /// Checks if existential is declared in this context.
    /// Solutions of existentials do not count.
    pub fn has_ex(&self, ex: Existential) -> bool {
        self.existentials.contains(&ex)
    }

    /// Seems to be a strange function, but useful in `ascend_ctx` checks.
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
    pub fn add_var(&mut self, var: TyVarId) {
        assert!(!self.vars.contains(&var));
        self.vars.push(var);
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

    /// Returns solution if existential is in context.
    pub fn solve(&mut self, ex: Existential, sol: Ty) -> Option<Ty> {
        if self.has_ex(ex) {
            assert!(
                self.solved.insert(ex.id(), sol).is_none(),
                "Tried to override solution of {} = {} to {}",
                ex,
                self.solved.get_unwrap(ex.id()),
                sol
            );
            Some(sol)
        } else {
            None
        }
    }

    pub fn add_func_param_sol(&mut self, ex: Existential, sol: Ty) -> Option<Ty> {
        if self.has_ex(ex) {
            // FIXME: Can function parameter existential be other than Common?
            match ex.kind() {
                ExistentialKind::Common => {},
                _ => panic!(),
            }

            if !self.func_param_solutions.has(ex.id()) {
                self.func_param_solutions.insert(ex.id(), vec![]);
            } else {
                // FIXME: Should we panic?
                assert!(!self.func_param_solutions.get_unwrap(ex.id()).contains(&sol));
            }

            self.func_param_solutions.get_mut_unwrap(ex.id()).push(sol);

            Some(sol)
        } else {
            None
        }
    }

    // Getters //
    pub fn unsolved(&self) -> Vec<Existential> {
        self.existentials
            .iter()
            .filter_map(|&ex| {
                if let None = self.get_solution(ex) {
                    Some(ex)
                } else {
                    None
                }
            })
            .collect()
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

    pub fn terms(&self) -> &HashMap<Symbol, Ty> {
        &self.terms
    }

    pub fn existentials(&self) -> &Vec<Existential> {
        &self.existentials
    }

    pub fn vars(&self) -> &Vec<TyVarId> {
        &self.vars
    }
}
