use std::collections::HashMap;

use crate::{hir::HirId, resolve::builtin::Builtin, span::span::Ident};

use super::{
    builtin::builtins,
    ty::{Existential, PrimTy, Ty, TyInterner, TyKind, TyS, DEFAULT_FLOAT_KIND, DEFAULT_INT_KIND},
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
        // self.typed.insert(id, ty).is_none();
    }

    pub fn node_type(&self, id: HirId) -> Option<Ty> {
        self.typed.get(&id).copied()
    }

    pub fn builtin_ty(&self, builtin: Builtin) -> Ty {
        self.builtins
            .get(&builtin)
            .copied()
            .expect(&format!("No type for {} builtin :(", builtin))
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

    pub fn prim(&mut self, prim: PrimTy) -> Ty {
        self.intern(TyS::new(TyKind::Prim(prim)))
    }

    pub fn func(&mut self, param: Ty, ret: Ty) -> Ty {
        self.intern(TyS::new(TyKind::Func(param, ret)))
    }

    pub fn forall(&mut self, alpha: Ident, body: Ty) -> Ty {
        self.intern(TyS::new(TyKind::Forall(alpha, body)))
    }

    pub fn existential(&mut self, ex: Existential) -> Ty {
        self.intern(TyS::new(TyKind::Existential(ex)))
    }

    pub fn default_int(&mut self) -> Ty {
        self.prim(PrimTy::Int(DEFAULT_INT_KIND))
    }

    pub fn default_float(&mut self) -> Ty {
        self.prim(PrimTy::Float(DEFAULT_FLOAT_KIND))
    }

    // Checks //
    pub fn is_instantiated(&self, ty: Ty) -> bool {
        match self.ty(ty).kind() {
            TyKind::Error => todo!(),
            TyKind::Unit
            | TyKind::Prim(PrimTy::Bool | PrimTy::Float(_) | PrimTy::Int(_) | PrimTy::String) => {
                true
            },
            TyKind::Var(_) | TyKind::Existential(_) | TyKind::Forall(_, _) => false,
            &TyKind::Func(param, body) => self.is_instantiated(param) && self.is_instantiated(body),
        }
    }

    pub fn is_mono(&self, ty: Ty) -> bool {
        match self.ty(ty).kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Prim(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => true,
            TyKind::Func(param, body) => self.is_mono(*param) && self.is_mono(*body),
            TyKind::Forall(_, _) => false,
        }
    }

    // Debug //
    pub fn pp(&self, ty: Ty) -> String {
        match self.ty(ty).kind() {
            TyKind::Error => format!("[ERROR]"),
            TyKind::Unit => format!("()"),
            TyKind::Prim(lit) => format!("{}", lit),
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
