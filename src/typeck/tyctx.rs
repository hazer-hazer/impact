use std::collections::HashMap;

use crate::{
    cli::verbose,
    hir::{expr::Expr, HirId},
    resolve::builtin::Builtin,
};

use super::{
    builtin::builtins,
    ctx::GlobalCtx,
    ty::{Subst, Ty, TyKind, TyVarId},
};

pub struct TyCtx {
    builtins: HashMap<Builtin, Ty>,

    /// Types associated to DefId's (meaning for each item is different!)
    /// and types of HIR expressions.
    /// - Type alias: `[Type alias DefId] -> [Its type]`
    /// - Declaration: `[Declaration DefId] -> [Type of assigned value]`
    typed: HashMap<HirId, Ty>,

    ty_bindings: HashMap<(Expr, TyVarId), Ty>,
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            builtins: builtins(),
            typed: Default::default(),
            ty_bindings: Default::default(),
        }
    }

    pub fn type_node(&mut self, id: HirId, ty: Ty) {
        verbose!("Type node ");
        // AGENDA
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

    pub fn instantiated_ty(&self, expr: Expr) -> Result<Ty, String> {
        self._instantiated_ty(expr, self.tyof(expr))
    }

    fn instantiated_ty_var(&self, expr: Expr, var: TyVarId) -> Result<Ty, String> {
        match self.ty_bindings.get(&(expr, var)) {
            Some(&ty) => {
                assert!(ty.is_mono());
                Ok(ty)
            },
            // TODO: Type variable name is a number, add namings!
            None => Err(format!("Cannot infer exact type of type variable {}", var)),
        }
    }

    fn _instantiated_ty(&self, expr: Expr, ty: Ty) -> Result<Ty, String> {
        match ty.kind() {
            TyKind::Error | TyKind::Unit | TyKind::Prim(_) => {
                Err(format!("{} is non-callable type", ty))
            },
            &TyKind::Var(var) => self.instantiated_ty_var(expr, var),
            // FIXME: Panic?
            TyKind::Existential(_) => panic!(),
            &TyKind::Func(param, body) => Ok(Ty::func(
                self._instantiated_ty(expr, param)?,
                self._instantiated_ty(expr, body)?,
            )),
            &TyKind::Forall(alpha, body) => {
                let alpha_ty = self.instantiated_ty_var(expr, alpha)?;
                Ok(body.substitute(Subst::Var(alpha), alpha_ty))
            },
        }
    }

    pub fn instantiated_func_ty(&self, expr: Expr) -> Result<Ty, String> {
        let ty = self.instantiated_ty(expr)?;
        match ty.kind() {
            // FIXME: Allow error???
            // TyKind::Error => todo!(),
            TyKind::Func(_, _) => Ok(ty),
            _ => Err(format!("{} is non-callable type", ty)),
        }
    }

    pub fn bind_ty_var(&mut self, expr: Expr, var: TyVarId, ty: Ty) {
        verbose!("Bind type variable {} = {}", var, ty);
        assert!(
            ty.is_mono(),
            "Cannot bind polytype {} to type variable {}",
            ty,
            var
        );
        assert!(
            self.ty_bindings.insert((expr, var), ty).is_none(),
            "Tried to rebind type variable {} in expression {} to {}",
            var,
            expr,
            ty
        );
    }

    pub fn ty_bindings(&self) -> &HashMap<(Expr, TyVarId), Ty> {
        &self.ty_bindings
    }

    // Debug //
    pub fn pp_typed_node(&self, id: HirId) -> String {
        self.node_type(id)
            .map_or("(?)".to_string(), |ty| format!("{}", ty))
    }
}
