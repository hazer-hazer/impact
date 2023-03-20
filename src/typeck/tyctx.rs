use std::collections::{HashMap, HashSet};

use crate::{
    cli::verbose,
    dt::idx::IndexVec,
    hir::{expr::Expr, HirId},
    resolve::{
        builtin::Builtin,
        def::{DefId, DefMap},
    },
};

use super::{
    builtin::builtins,
    ctx::GlobalCtx,
    ty::{Subst, Ty, TyKind, TyVarId},
};

#[derive(Debug, Default, PartialEq)]
pub struct TyBindings(IndexVec<TyVarId, Option<Ty>>);

impl TyBindings {
    fn substitution_for(&self, var: TyVarId) -> Option<Ty> {
        self.0.get_flat(var).copied()
    }

    fn bind(&mut self, var: TyVarId, ty: Ty) -> Option<Ty> {
        self.0.insert(var, ty)
    }

    pub fn iter(&self) -> impl Iterator<Item = (TyVarId, &Ty)> + '_ {
        self.0.iter_enumerated_flat()
    }
}

pub struct TyCtx {
    builtins: HashMap<Builtin, Ty>,

    /// Types associated to DefId's (meaning for each item is different!)
    /// and types of HIR expressions.
    /// - Type alias: `[Type alias DefId] -> [Its type]`
    /// - Declaration: `[Declaration DefId] -> [Type of assigned value]`
    typed: HashMap<HirId, Ty>,

    expr_ty_bindings: HashMap<Expr, TyBindings>,

    def_ty_bindings: DefMap<HashSet<Expr>>,
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            builtins: builtins(),
            typed: Default::default(),
            expr_ty_bindings: Default::default(),
            def_ty_bindings: Default::default(),
        }
    }

    pub fn type_node(&mut self, id: HirId, ty: Ty) {
        verbose!("Type node {} with {}", id, ty);
        assert!(
            self.typed.insert(id, ty).is_none(),
            "{} is already typed with {}",
            id,
            ty
        );
        // self.typed.insert(id, ty).is_none();
    }

    pub fn apply_ctx_on_typed_nodes(&mut self, ctx: &GlobalCtx) {
        self.typed.iter_mut().for_each(|(_, ty)| {
            *ty = ctx.apply_on(*ty);
        });
    }

    pub fn builtin(&self, bt: Builtin) -> Ty {
        self.builtins.get(&bt).copied().unwrap()
    }

    pub fn node_type(&self, id: HirId) -> Option<Ty> {
        self.typed.get(&id).copied()
    }

    /// Unwrap-version of `node_type` with a short name.
    pub fn tyof(&self, id: HirId) -> Ty {
        self.node_type(id)
            .expect(&format!("Type of node {} expected", id))
    }

    pub fn instantiated_expr_ty(&self, expr: Expr) -> Result<Ty, String> {
        self._instantiated_ty(expr, self.tyof(expr))
    }

    fn _instantiated_ty(&self, expr: Expr, ty: Ty) -> Result<Ty, String> {
        match ty.kind() {
            TyKind::Error => todo!(),
            TyKind::Unit | TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) | TyKind::String => {
                Ok(ty)
            },
            &TyKind::Var(var) => self.instantiated_expr_ty_var(expr, var),
            // FIXME: Panic?
            TyKind::Existential(_) => panic!(),
            &TyKind::Func(param, body) | &TyKind::FuncDef(_, param, body) => Ok(Ty::func(
                ty.func_def_id(),
                self._instantiated_ty(expr, param)?,
                self._instantiated_ty(expr, body)?,
            )),
            &TyKind::Forall(alpha, body) => {
                let alpha_ty = self.instantiated_expr_ty_var(expr, alpha)?;
                Ok(body.substitute(Subst::Var(alpha), alpha_ty))
            },
        }
    }

    fn get_binding(&self, expr: Expr, var: TyVarId) -> Option<Ty> {
        self.expr_ty_bindings.get(&expr)?.substitution_for(var)
    }

    fn instantiated_expr_ty_var(&self, expr: Expr, var: TyVarId) -> Result<Ty, String> {
        match self.get_binding(expr, var) {
            Some(ty) => {
                assert!(ty.is_mono());
                Ok(ty)
            },
            // TODO: Type variable name is a number, add namings!
            None => Err(format!(
                "Cannot infer exact type of type variable {:?}",
                var
            )),
        }
    }

    pub fn instantiated_func_ty(&self, expr: Expr) -> Result<Ty, String> {
        let ty = self.instantiated_expr_ty(expr)?;
        match ty.kind() {
            // FIXME: Allow error???
            // TyKind::Error => todo!(),
            TyKind::Func(..) | TyKind::FuncDef(..) => Ok(ty),
            _ => Err(format!("{} is non-callable type", ty)),
        }
    }

    pub fn bind_ty_var(&mut self, def_id: Option<DefId>, expr: Expr, var: TyVarId, ty: Ty) {
        verbose!("Bind type variable {} = {}", var, ty);
        assert!(
            ty.is_mono(),
            "Cannot bind polytype {} to type variable {}",
            ty,
            var
        );
        assert!(
            self.expr_ty_bindings
                .entry(expr)
                .or_default()
                .bind(var, ty)
                .is_none(),
            "Tried to rebind type variable {} in expression {} to {}",
            var,
            expr,
            ty
        );

        if let Some(def_id) = def_id {
            assert!(self.def_ty_bindings.upsert_default(def_id).insert(expr));
        }
    }

    pub fn expr_ty_bindings(&self) -> &HashMap<Expr, TyBindings> {
        &self.expr_ty_bindings
    }

    /// Get list of expressions with unique types bound in definition.
    /// I.e., having function `id :: forall x. x` and two calls `id 1` and `id 2`,
    /// we'll get list with expression id of one of this calls, because `x` is bound only to some int.
    /// Then, we can get substitutions for this definitions from `expr_ty_bindings`.
    /// We have `Expr -> (TyVarId -> Ty)[]` and `DefId -> set Expr` mapping.
    /// The result must be a list of expressions with unique substitutions of definition type variables.
    pub fn unique_def_bound_usages(&self, def_id: DefId) -> Vec<Expr> {
        let mut set = HashMap::<(TyVarId, Ty), Expr>::new();
        let _a = self
            .def_ty_bindings
            .get_unwrap(def_id)
            .iter()
            .for_each(|expr| {
                self.expr_ty_bindings
                    .get(expr)
                    .unwrap()
                    .iter()
                    .for_each(|(var, ty)| {
                        set.insert((var, *ty), *expr);
                    });
            });

        set.values().copied().collect()
    }

    // Debug //
    pub fn pp_typed_node(&self, id: HirId) -> String {
        self.node_type(id)
            .map_or("(?)".to_string(), |ty| format!("{}", ty))
    }
}
