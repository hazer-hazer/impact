// Ty methods used only in typeck and unavailable outside of typeck

use std::{
    collections::HashSet,
    fmt::Formatter,
    hash::{Hash, Hasher},
};

use self::interner::TY_INTERNER;
use super::{
    ctx::AlgoCtx,
    kind::MonoKind,
    ty::{Adt, FloatKind, IntKind, MapTy, Struct},
    Ty,
};
use crate::{
    cli::verbose,
    dt::idx::Idx,
    resolve::def::DefId,
    span::sym::Ident,
    typeck::{
        kind::{Kind, KindEx, KindSort},
        ty::{Ex, TyKind, TyVarId},
    }, session::MaybeWithSession,
};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct TyS {
    kind: TyKind,
    // kind: Kind,
}

impl TyS {
    pub fn new(kind: TyKind) -> Self {
        Self { kind }
    }

    pub fn error() -> Self {
        Self::new(TyKind::Error)
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }
}

impl std::fmt::Display for TyS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl Ty {
    pub fn intern(tys: TyS) -> Self {
        Self(TY_INTERNER.write().unwrap().intern(tys))
    }

    pub fn new(kind: TyKind) -> Self {
        Self::intern(TyS::new(kind))
    }

    pub fn tys(&self) -> &TyS {
        TY_INTERNER.read().unwrap().expect(self.id())
    }

    pub fn ty_var_name_or_id(var: TyVarId) -> String {
        TY_INTERNER
            .read()
            .unwrap()
            .ty_var_name(var)
            .map(|name| name.to_string())
            .unwrap_or(format!("a{}", var))
    }

    pub fn next_ty_var_id(name: Option<Ident>) -> TyVarId {
        TY_INTERNER.write().unwrap().next_ty_var(name)
    }

    pub fn maybe_add_func_def_id(&self, def_id: DefId) -> Ty {
        match self.kind() {
            TyKind::FuncDef(..) => panic!("Function DefId cannot be set type FuncDef"),
            TyKind::Func(params, body) => Ty::func(Some(def_id), params.clone(), *body),
            _ => *self,
        }
    }

    // Getters //
    pub fn as_ex(&self) -> Option<Ex> {
        match self.kind() {
            &TyKind::Existential(ex) => Some(ex),
            _ => None,
        }
    }

    pub fn expect_kind(&self) -> Kind {
        match self.kind() {
            &TyKind::Kind(kind) => kind,
            _ => panic!(),
        }
    }

    pub fn as_kind_ex(&self) -> Option<KindEx> {
        match self.kind() {
            TyKind::Kind(kind) => match kind.sort() {
                &KindSort::Ex(ex) => Some(ex),
                _ => None,
            },
            _ => None,
        }
    }

    /// Returns type variables appeared in type or type variable itself
    pub fn inner_ty_vars(&self) -> HashSet<TyVarId> {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Existential(_)
            | TyKind::Str => HashSet::default(),
            TyKind::FuncDef(_, params, body) | TyKind::Func(params, body) => params
                .into_iter()
                .chain([body].into_iter())
                .map(Ty::inner_ty_vars)
                .flatten()
                .collect(),
            TyKind::Adt(adt) => adt
                .walk_tys()
                .map(|ty| ty.inner_ty_vars())
                .flatten()
                .collect(),
            TyKind::Struct(data) => data.walk_tys().map(|ty| ty.inner_ty_vars()).flatten().collect(),
            TyKind::Ref(ty) => ty.inner_ty_vars(),
            &TyKind::Var(var) => HashSet::from([var]),
            // Forall type variable is ignored, as might be unused in body
            TyKind::Forall(_, ty) => ty.inner_ty_vars(),
            TyKind::Kind(kind) => kind.get_ty_vars(),
        }
    }

    /// Returns vec of type variables from outer `forall`s.
    // FIXME: Unused
    pub fn outer_ty_vars(&self) -> Vec<TyVarId> {
        match self.kind() {
            &TyKind::Forall(var, body) => vec![var]
                .into_iter()
                .chain(body.outer_ty_vars().into_iter())
                .collect(),
            _ => vec![],
        }
    }

    pub fn walk_inner_tys(&self) -> Box<dyn Iterator<Item = Ty> + '_> {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Existential(_)
            | TyKind::Var(_)
            | TyKind::Str => Box::new(std::iter::empty()),
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                Box::new(params.iter().copied().chain(std::iter::once(*body)))
            },
            TyKind::Adt(adt) => Box::new(adt.walk_tys()),
            TyKind::Struct(data) => Box::new(data.walk_tys()),
            TyKind::Ref(ty) => ty.walk_inner_tys(),
            TyKind::Forall(_, body) => body.walk_inner_tys(),
            TyKind::Kind(kind) => kind.walk_inner_tys(),
        }
    }

    // Checks //
    pub fn is_ty(&self) -> bool {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::FuncDef(..)
            | TyKind::Func(_, _)
            | TyKind::Adt(..)
            | TyKind::Ref(_)
            // TODO: Can existentials and vars be higher-kinded?
            | TyKind::Var(_)
            | TyKind::Existential(_)|
            TyKind::Struct(_) 
            | TyKind::Forall(_, _) => true,
            TyKind::Kind(_) => false,
        }
    }

    pub fn is_kind(&self) -> bool {
        matches!(self.kind(), TyKind::Kind(_))
    }

    pub fn assert_is_ty(&self) {
        assert!(self.is_ty(), "Expected a type, got {}", self.without_sess());
    }

    pub fn is_solved(&self) -> bool {
        match self.kind() {
            TyKind::Error => true,
            TyKind::Unit | TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) | TyKind::Str => true,
            TyKind::FuncDef(_, params, body) | TyKind::Func(params, body) => {
                params.iter().all(Ty::is_solved) && body.is_solved()
            },
            TyKind::Adt(adt) => adt.walk_tys().all(|ty| ty.is_solved()),
            TyKind::Struct(data) => data.walk_tys().all(|ty| ty.is_solved()),
            // TODO: Check that var is bound in ty_bindings?
            TyKind::Var(_) => true,
            TyKind::Existential(_) => false,
            TyKind::Forall(_, ty) => ty.is_solved(),
            TyKind::Ref(ty) => ty.is_solved(),
            TyKind::Kind(kind) => kind.is_solved(),
        }
    }

    pub fn contains_ex(&self, ex: Ex) -> bool {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Var(_) => false,
            &TyKind::Existential(ex_) => ex == ex_,
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                params.iter().copied().any(|param| param.contains_ex(ex)) || body.contains_ex(ex)
            },
            &TyKind::Forall(_, body) => body.contains_ex(ex),
            &TyKind::Ref(inner) => inner.contains_ex(ex),
            TyKind::Kind(kind) => kind.contains_ty_ex(ex),
            TyKind::Adt(adt) => adt.walk_tys().any(|ty| ty.contains_ex(ex)),
            TyKind::Struct(data) => data.walk_tys().any(|ty| ty.contains_ex(ex)),
        }
    }

    pub fn contains_error(&self) -> bool {
        match self.kind() {
            TyKind::Error => true,
            _ => self.walk_inner_tys().any(|ty| ty.contains_error()),
        }
    }

    // Modifiers //
    pub fn degeneralize(&self) -> Ty {
        match self.kind() {
            TyKind::Forall(_, body) => body.degeneralize(),
            _ => *self,
        }
    }

    pub fn substitute(&self, subst: TyVarId, with: Ty) -> Ty {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str => *self,
            &TyKind::Var(ident) => {
                if subst == ident {
                    with
                } else {
                    *self
                }
            },
            &TyKind::Existential(_ex) => todo!(),
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                let param = params
                    .iter()
                    .map(|param| param.substitute(subst, with))
                    .collect();
                let body = body.substitute(subst, with);
                Ty::func(self.func_def_id(), param, body)
            },
            &TyKind::Ref(ty) => Ty::ref_to(ty.substitute(subst, with)),
            &TyKind::Forall(ident, body) => {
                if subst == ident {
                    Ty::forall(ident, with)
                } else {
                    let subst = body.substitute(subst, with);
                    Ty::forall(ident, subst)
                }
            },
            TyKind::Kind(kind) => Ty::ty_kind(kind.substitute_ty(subst, Kind::new_ty(with))),
            TyKind::Adt(adt) => Ty::adt(adt.map_ty_pure(&mut |ty| Ok(ty.substitute(subst, with)))),
            TyKind::Struct(data) => Ty::struct_(data.map_ty_pure(&mut |ty| Ok(ty.substitute(subst, with)))),
        }
    }

    /// Substitution applied to type such that `forall a1. forall aN.
    /// body`/`subst` becomes `forall a1. forall aN. subst`.
    pub fn substituted_forall_body(&self, subst: Ty) -> Ty {
        match self.kind() {
            &TyKind::Forall(var, body) => Ty::forall(var, body.substituted_forall_body(subst)),
            _ => subst,
        }
    }

    // MonoTy API //
    pub fn as_mono(self) -> Option<MonoTy> {
        self.try_into().ok()
    }

    pub fn mono(self) -> MonoTy {
        self.as_mono()
            .expect(&format!("{} expected to be a mono type", self.without_sess()))
    }

    pub fn mono_checked(self) -> Self {
        assert!(self.is_mono());
        self
    }

    // Working with context //
    pub fn apply_ctx(self, ctx: &impl AlgoCtx) -> Ty {
        let ty = self._apply_ctx(ctx);
        if self != ty {
            verbose!("Applying ctx on type it is solved as {} => {}", self.without_sess(), ty.without_sess());
        }
        ty
    }

    pub fn _apply_ctx(&self, ctx: &impl AlgoCtx) -> Ty {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Var(_)
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str => *self,

            &TyKind::Existential(ex) => ctx.get_solution(ex).map_or(*self, |ty| ty.apply_ctx(ctx)),

            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                let params = params
                    .iter()
                    .copied()
                    .map(|param| param.apply_ctx(ctx))
                    .collect();
                let body = body.apply_ctx(ctx);
                Ty::func(self.func_def_id(), params, body)
            },

            &TyKind::Forall(ident, body) => Ty::forall(ident, body.apply_ctx(ctx)),
            &TyKind::Ref(inner) => Ty::ref_to(inner.apply_ctx(ctx)),
            TyKind::Kind(kind) => {
                let kind = kind.apply_ctx(ctx);
                match kind.sort() {
                    &KindSort::Ty(ty) => ty,
                    _ => Ty::ty_kind(kind),
                }
            },
            TyKind::Adt(adt) => Ty::adt(adt.map_ty_pure(&mut |ty| Ok(ty._apply_ctx(ctx)))),
            TyKind::Struct(data) => Ty::struct_(data.map_ty_pure(&mut |ty| Ok(ty._apply_ctx(ctx)))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MonoTyKind {
    Error,
    Unit,
    Bool,
    Int(IntKind),
    Float(FloatKind),
    String,
    Ex(Ex),
    Var(TyVarId),
    Func(Vec<Box<MonoTy>>, Box<MonoTy>),
    Ref(Box<MonoTy>),
    Adt(Adt<MonoTy>),
    Struct(Struct<MonoTy>),
    Kind(Box<MonoKind>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MonoTy {
    pub sort: MonoTyKind,
    pub ty: Ty,
}

impl TryFrom<Ty> for MonoTy {
    type Error = ();

    fn try_from(value: Ty) -> Result<Self, Self::Error> {
        let sort = match value.kind() {
            TyKind::Error => Some(MonoTyKind::Error),
            TyKind::Unit => Some(MonoTyKind::Unit),
            TyKind::Bool => Some(MonoTyKind::Bool),
            &TyKind::Int(kind) => Some(MonoTyKind::Int(kind)),
            &TyKind::Float(kind) => Some(MonoTyKind::Float(kind)),
            TyKind::Str => Some(MonoTyKind::String),
            &TyKind::Ref(ty) => Some(MonoTyKind::Ref(Box::new(ty.try_into()?))),
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                let params = params
                    .iter()
                    .map(|&param| param.try_into())
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(Box::new)
                    .collect();
                let body = Self::try_from(*body)?;
                Some(MonoTyKind::Func(params, Box::new(body)))
            },
            TyKind::Adt(adt) => Some(MonoTyKind::Adt(adt.map_ty(&mut |ty| ty.try_into())?)),
            TyKind::Struct(data) => Some(MonoTyKind::Struct(data.map_ty(&mut |ty| ty.try_into())?)),
            &TyKind::Existential(ex) => Some(MonoTyKind::Ex(ex)),
            &TyKind::Var(var) => Some(MonoTyKind::Var(var)),
            &TyKind::Kind(kind) => Some(MonoTyKind::Kind(Box::new(kind.try_into()?))),
            TyKind::Forall(..) => None,
        }
        .ok_or(())?;

        Ok(MonoTy { sort, ty: value })
    }
}

mod interner {
    use std::{
        collections::{hash_map::DefaultHasher, HashMap},
        hash::{Hash, Hasher},
        sync::RwLock,
    };

    use once_cell::sync::Lazy;

    use super::TyS;
    use crate::{
        dt::idx::{Idx, IndexVec},
        span::sym::Ident,
        typeck::ty::{TyId, TyVarId},
    };

    pub static TY_INTERNER: Lazy<RwLock<TyInterner>> = Lazy::new(|| RwLock::new(TyInterner::new()));

    pub struct TyInterner {
        map: HashMap<u64, TyId>,
        types: Vec<&'static TyS>,
        ty_var_id: TyVarId,
        ty_var_names: IndexVec<TyVarId, Option<Ident>>,
    }

    impl TyInterner {
        pub fn new() -> Self {
            Self {
                map: Default::default(),
                types: Default::default(),
                ty_var_id: TyVarId::new(0),
                ty_var_names: Default::default(),
            }
        }

        fn hash(ty: &TyS) -> u64 {
            let mut state = DefaultHasher::new();
            ty.hash(&mut state);
            state.finish()
        }

        pub fn intern(&mut self, tys: TyS) -> TyId {
            let hash = Self::hash(&tys);

            if let Some(id) = self.map.get(&hash) {
                return *id;
            }

            // !Leaked
            let ty = Box::leak(Box::new(tys));
            let id = TyId::from(self.types.len());

            self.map.insert(hash, id);
            self.types.push(ty);

            id
        }

        pub fn expect(&self, ty: TyId) -> &'static TyS {
            self.types
                .get(ty.as_usize())
                .expect(&format!("Failed to find type by type id {}", ty))
        }

        pub fn next_ty_var(&mut self, name: Option<Ident>) -> TyVarId {
            let id = *self.ty_var_id.inc();
            if let Some(name) = name {
                assert!(self.ty_var_names.insert(id, name).is_none());
            }
            id
        }

        pub fn ty_var_name(&self, id: TyVarId) -> Option<Ident> {
            self.ty_var_names.get_flat(id).copied()
        }
    }
}
