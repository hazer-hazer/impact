use std::collections::HashMap;

use crate::{
    cli::verbose,
    hir::{expr::Expr, HirId},
    resolve::builtin::Builtin,
};

use super::{
    builtin::builtins,
    ty::{Ty, TyVarId},
};

pub struct TyCtx {
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
            typed: Default::default(),
            ty_bindings: Default::default(),
        }
    }

    pub fn type_node(&mut self, id: HirId, ty: Ty) {
        assert!(self.typed.insert(id, ty).is_none());
        // self.typed.insert(id, ty).is_none();
    }

    pub fn node_type(&self, id: HirId) -> Option<Ty> {
        self.typed.get(&id).copied()
    }

    /// Unwrap-version of `node_type` with a short name.
    pub fn tyof(&self, id: HirId) -> Ty {
        self.node_type(id)
            .expect(&format!("Type of node {} expected", id))
    }

    pub fn bind_ty_var(&mut self, expr: Expr, var: TyVarId, ty: Ty) {
        verbose!("Bind type variable {} = {}", var, ty);
        assert!(ty.is_mono());
        assert!(self.ty_bindings.insert((expr, var), ty).is_none());
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
