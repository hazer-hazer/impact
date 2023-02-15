use std::collections::HashMap;

use crate::{hir::HirId, resolve::builtin::Builtin};

use super::{builtin::builtins, ty::Ty};

pub struct TyCtx {
    /// Just a quick-access map
    builtins: HashMap<Builtin, Ty>,

    /// Types associated to DefId's (meaning for each item is different!)
    /// and types of HIR expressions.
    /// - Type alias: `[Type alias DefId] -> [Its type]`
    /// - Declaration: `[Declaration DefId] -> [Type of assigned value]`
    typed: HashMap<HirId, Ty>,
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            typed: Default::default(),
            builtins: builtins(),
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
        self.node_type(id).expect(&format!("Type of node {} expected", id))
    }

    pub fn builtin_ty(&self, builtin: Builtin) -> Ty {
        self.builtins
            .get(&builtin)
            .copied()
            .expect(&format!("No type for {} builtin :(", builtin))
    }

    // Debug //
    pub fn pp_typed_node(&self, id: HirId) -> String {
        self.node_type(id)
            .map_or("(?)".to_string(), |ty| format!("{}", ty))
    }
}
