use std::collections::HashMap;

use super::{
    kind::{Kind, KindEx, KindExId, KindVarId},
    ty::{Ex, ExId, ExKind, MonoTy, Ty, TyVarId},
};
use crate::{
    cli::verbose,
    dt::idx::IndexVec,
    hir::HirId,
    span::sym::{Ident, Symbol},
};

pub trait AlgoCtx {
    fn get_solution(&self, ex: Ex) -> Option<Ty>;
    fn get_kind_ex_solution(&self, ex: KindEx) -> Option<Kind>;
}

#[derive(Default, Debug)]
pub struct GlobalCtx {
    solved: IndexVec<ExId, Option<Ty>>,

    // FIXME: This might be absolutely wrong
    existentials: IndexVec<ExId, Option<(usize, usize)>>,

    solved_kind_exes: IndexVec<KindExId, Option<Kind>>,
    kind_exes: IndexVec<KindExId, Option<(usize, usize)>>,
}

impl AlgoCtx for GlobalCtx {
    fn get_solution(&self, ex: Ex) -> Option<Ty> {
        self.solved.get_flat(ex.id()).copied()
    }

    fn get_kind_ex_solution(&self, ex: KindEx) -> Option<Kind> {
        self.solved_kind_exes.get_flat(ex.id()).copied()
    }
}

impl GlobalCtx {
    /// Add context, mostly after popping it from stack.
    /// When we exit context, all existentials need to be saved
    /// because we need them to get a solution for solved types
    /// having either successfully inferred type or a typeck error.
    pub fn add(&mut self, depth: usize, ctx: InferCtx) {
        ctx.solved().for_each(|(ex, &sol)| {
            verbose!("Extract solution {ex} = {sol} to global ctx from [CTX {depth}]");
            assert!(self.solved.insert(ex, sol).is_none());
        });

        ctx.solved_kind_exes().for_each(|(ex, &sol)| {
            verbose!("Extract solution {ex} = {sol} to global ctx from [CTX {depth}]");
            assert!(self.solved_kind_exes.insert(ex, sol).is_none());
        });

        ctx.existentials()
            .iter()
            .enumerate()
            .for_each(|(index, &ex)| {
                assert!(self.existentials.insert(ex.id(), (depth, index)).is_none())
            });
    }

    pub fn has_ex(&self, ex: Ex) -> bool {
        self.existentials().contains(&ex.id()) || self.solved.has(ex.id())
    }

    pub fn has_kind_ex(&self, ex: KindEx) -> bool {
        self.kind_exes
            .iter_enumerated_flat()
            .any(|(ex_, _)| ex.id() == ex_)
            || self.solved_kind_exes.has(ex.id())
    }

    pub fn get_ex_index(&self, ex: Ex) -> Option<(usize, usize)> {
        self.existentials.get_flat(ex.id()).copied()
    }

    pub fn get_kind_ex_index(&self, ex: KindEx) -> Option<(usize, usize)> {
        self.kind_exes.get_flat(ex.id()).copied()
    }

    pub fn solved(&self) -> &IndexVec<ExId, Option<Ty>> {
        &self.solved
    }

    // FIXME: Copypaste
    pub fn solve(&mut self, ex: Ex, sol: Ty) -> Option<Ty> {
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

    pub fn existentials(&self) -> Vec<ExId> {
        self.existentials
            .iter_enumerated_flat()
            .map(|(ex, _)| ex)
            .collect()
    }

    pub fn unsolved(&self) -> Vec<ExId> {
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

/// InferCtx is a common abstraction similar to algorithmic context
/// from CaEBTC.
///
/// TODO: Possible optimization is to split `InferCtx` to two kinds:
/// `KindInferCtx` and `TyInferCtx`. TODO:  Because kind existentials and
/// variables do not exists on the same context frame at one moment.
#[derive(Default, Clone, Debug)]
pub struct InferCtx {
    /// Existentials defined in current context
    existentials: Vec<Ex>,

    /// Existentials solved in current context. Leak to global context.
    // Note: I end up with storing `solved` and unsolved existentials separately due to `try`
    // logic.  We need to enter new "try to"-context under which we do not violate upper
    // context.
    solved: IndexVec<ExId, Option<Ty>>,

    /// Type variables defined in current context. Do not leak to global
    /// context. Actually only used for well-formedness checks.
    vars: Vec<TyVarId>,

    /// Kind variables defined in current context. Do not leak to global
    /// context. As type variables only used for well-formedness checks.
    kind_vars: Vec<KindVarId>,

    /// Kind existentials defined in current context.
    kind_exes: Vec<KindEx>,

    /// Kind existentials solved in current context. Have same properties as
    /// `solved`.
    solved_kind_exes: IndexVec<KindExId, Option<Kind>>,
    // // TODO: Might be only Expr nodes
    // inferring_nodes: Vec<HirId>,
}

impl AlgoCtx for InferCtx {
    fn get_solution(&self, ex: Ex) -> Option<Ty> {
        self.solved.get_flat(ex.id()).copied()
    }

    fn get_kind_ex_solution(&self, ex: KindEx) -> Option<Kind> {
        self.solved_kind_exes.get_flat(ex.id()).copied()
    }
}

impl InferCtx {
    // Constructors //
    pub fn new_with_var(var: TyVarId) -> Self {
        Self {
            vars: Vec::from([var]),
            ..Default::default()
        }
    }

    pub fn new_with_ex(ex: Ex) -> Self {
        Self {
            existentials: Vec::from([ex]),
            ..Default::default()
        }
    }

    pub fn new_with_kind_ex(ex: KindEx) -> Self {
        Self {
            kind_exes: Vec::from([ex]),
            ..Default::default()
        }
    }

    pub fn new_with_kind_var(var: KindVarId) -> Self {
        Self {
            kind_vars: Vec::from([var]),
            ..Default::default()
        }
    }

    // pub fn add_inferring_node(&mut self, id: HirId) {
    //     // assert!(!self.inferring_nodes.contains(&id));
    //     self.inferring_nodes.push(id);
    // }

    // pub fn inferring_nodes(&self) -> &[HirId] {
    //     self.inferring_nodes.as_ref()
    // }

    // Getters //
    pub fn get_var(&self, var: TyVarId) -> Option<TyVarId> {
        self.vars.iter().find(|&&_var| var == _var).copied()
    }

    pub fn get_ex_index(&self, ex: Ex) -> Option<usize> {
        self.existentials.iter().position(|&ex_| ex == ex_)
    }

    pub fn get_kind_ex_index(&self, ex: KindEx) -> Option<usize> {
        self.kind_exes.iter().position(|&ex_| ex == ex_)
    }

    /// Checks if existential is declared in this context.
    /// Solutions of existentials do not count.
    pub fn has_ex(&self, ex: Ex) -> bool {
        self.existentials.contains(&ex)
    }

    pub fn has_kind_ex(&self, ex: KindEx) -> bool {
        self.kind_exes.contains(&ex)
    }

    /// Seems to be a strange function, but useful in `ascend_ctx` checks.
    pub fn get_ex(&self, ex: Ex) -> Option<Ex> {
        if self.has_ex(ex) {
            Some(ex)
        } else {
            None
        }
    }

    pub fn get_kind_ex(&self, ex: KindEx) -> Option<KindEx> {
        if self.has_kind_ex(ex) {
            Some(ex)
        } else {
            None
        }
    }

    // Setters //
    pub fn add_var(&mut self, var: TyVarId) {
        assert!(!self.vars.contains(&var));
        self.vars.push(var);
    }

    pub fn add_ex(&mut self, ex: Ex) {
        assert!(!self.has_ex(ex));
        self.existentials.push(ex);
    }

    pub fn add_kind_ex(&mut self, ex: KindEx) {
        assert!(!self.has_kind_ex(ex));
        self.kind_exes.push(ex);
    }

    /// Returns solution if existential is in context.
    pub fn solve(&mut self, ex: Ex, sol: MonoTy) {
        let sol = sol.ty;
        assert!(
            self.solved.insert(ex.id(), sol).is_none(),
            "Tried to override solution of {} = {} to {}",
            ex,
            self.solved.get_unwrap(ex.id()),
            sol
        );
    }

    pub fn solve_kind_ex(&mut self, ex: KindEx, sol: Kind) {
        // TODO: Can existentials out of context be solved here?
        assert!(self.solved_kind_exes.insert(ex.id(), sol).is_none());
    }

    // Getters //
    pub fn unsolved(&self) -> Vec<Ex> {
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

    pub fn solved(&self) -> impl Iterator<Item = (ExId, &Ty)> {
        self.solved.iter_enumerated_flat()
    }

    pub fn solved_kind_exes(&self) -> impl Iterator<Item = (KindExId, &Kind)> {
        self.solved_kind_exes.iter_enumerated_flat()
    }

    pub fn int_exes(&self) -> Vec<Ex> {
        self.existentials()
            .iter()
            .filter_map(|&ex| match ex.kind() {
                ExKind::Int => Some(ex),
                _ => None,
            })
            .collect()
    }

    pub fn float_exes(&self) -> Vec<Ex> {
        self.existentials()
            .iter()
            .filter_map(|&ex| match ex.kind() {
                ExKind::Float => Some(ex),
                _ => None,
            })
            .collect()
    }

    pub fn existentials(&self) -> &Vec<Ex> {
        &self.existentials
    }

    pub fn vars(&self) -> &Vec<TyVarId> {
        &self.vars
    }
}
