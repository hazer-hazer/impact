use std::collections::HashMap;

use crate::{
    cli::verbose,
    hir::HirId,
    resolve::builtin::Builtin,
    span::span::{Ident, Symbol},
};

use super::{
    builtin::builtins,
    ctx::ExistentialId,
    ty::{PrimTy, Ty, TyInterner, TyKind, TyS},
};

pub struct TyCtx {
    interner: TyInterner,

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
        let mut this = Self {
            interner: Default::default(),
            typed: Default::default(),
            builtins: Default::default(),
        };

        this.builtins = builtins(&mut this);

        this
    }

    pub fn ty(&self, ty: Ty) -> &TyS {
        self.interner.expect(ty)
    }

    pub fn type_node(&mut self, id: HirId, ty: Ty) {
        assert!(self.typed.insert(id, ty).is_none());
    }

    pub fn node_type(&self, id: HirId) -> Option<Ty> {
        self.typed.get(&id).copied()
    }

    pub fn builtin_ty(&self, builtin: Builtin) -> Ty {
        self.builtins.get(&builtin).copied().unwrap()
    }

    // Interning //
    pub fn intern(&mut self, ty: TyS) -> Ty {
        self.interner.intern(ty)
    }

    pub fn unit(&mut self) -> Ty {
        self.intern(TyS::new(TyKind::Unit))
    }

    pub fn error(&mut self) -> Ty {
        self.intern(TyS::new(TyKind::Error))
    }

    pub fn var(&mut self, ident: Ident) -> Ty {
        self.intern(TyS::new(TyKind::Var(ident)))
    }

    pub fn lit(&mut self, prim: PrimTy) -> Ty {
        self.intern(TyS::new(TyKind::Lit(prim)))
    }

    pub fn func(&mut self, param: Ty, ret: Ty) -> Ty {
        self.intern(TyS::new(TyKind::Func(param, ret)))
    }

    pub fn forall(&mut self, alpha: Ident, body: Ty) -> Ty {
        self.intern(TyS::new(TyKind::Forall(alpha, body)))
    }

    pub fn existential(&mut self, ex: ExistentialId) -> Ty {
        self.intern(TyS::new(TyKind::Existential(ex)))
    }

    // Helpers //
    pub fn is_mono(&self, ty: Ty) -> bool {
        match self.ty(ty).kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Lit(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => true,
            TyKind::Func(param_ty, return_ty) => {
                self.is_mono(*param_ty) && self.is_mono(*return_ty)
            },
            TyKind::Forall(_, _) => false,
        }
    }

    // Debug //
    pub fn pp(&self, ty: Ty) -> String {
        match self.ty(ty).kind() {
            TyKind::Error => format!("[ERROR]"),
            TyKind::Unit => format!("()"),
            TyKind::Lit(lit) => format!("{}", lit),
            TyKind::Var(name) => format!("{}", name),
            TyKind::Existential(ex) => format!("{}", ex),
            &TyKind::Func(param, body) => format!("({} -> {})", self.pp(param), self.pp(body)),
            &TyKind::Forall(alpha, ty) => format!("(∀{}. {})", alpha, self.pp(ty)),
        }
    }

    pub fn pp_typed_node(&self, id: HirId) -> String {
        self.node_type(id).map_or(format!("(?)"), |ty| self.pp(ty))
    }
}
