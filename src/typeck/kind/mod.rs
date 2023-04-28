pub mod check;

use std::{
    collections::HashSet,
    fmt::Display,
    hash::{Hash, Hasher},
};

use self::interner::KIND_INTERNER;
use super::{
    ctx::AlgoCtx,
    ty::{Ex, TyVarId},
};
use crate::{
    cli::color::{Color, ColorizedStruct},
    dt::idx::{declare_idx, Idx},
    span::sym::Ident,
    typeck::ty::{Ty, TyKind},
};

declare_idx!(KindId, u32, "{}", Color::White);
declare_idx!(KindExId, u32, "'^{}", Color::BrightMagenta);
declare_idx!(KindVarId, u32, "'{}", Color::BrightCyan);

impl KindVarId {
    pub fn pretty(&self) -> String {
        KIND_INTERNER
            .read()
            .unwrap()
            .kind_var_name(*self)
            .map(|name| name.to_string())
            .unwrap_or(format!("'k{}", self.0))
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct KindEx(KindExId);

impl KindEx {
    pub fn new(id: KindExId) -> Self {
        Self(id)
    }

    pub fn id(&self) -> KindExId {
        self.0
    }
}

impl Display for KindEx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Kind(KindId);

impl Kind {
    fn intern(data: KindData) -> Self {
        Self(KIND_INTERNER.write().unwrap().intern(data))
    }

    pub fn next_kind_var_id(name: Option<Ident>) -> KindVarId {
        KIND_INTERNER.write().unwrap().next_kind_var_id(name)
    }

    // Constructors //
    fn new(kind: KindSort) -> Self {
        Self::intern(KindData { sort: kind })
    }

    pub fn new_ty(ty: Ty) -> Self {
        assert!(!matches!(ty.kind(), TyKind::Kind(_)));
        Self::new(KindSort::Ty(ty))
    }

    pub fn new_abs(param: Kind, body: Kind) -> Self {
        Self::new(KindSort::Abs(param, body))
    }

    pub fn new_var(var: KindVarId) -> Self {
        Self::new(KindSort::Var(var))
    }

    pub fn new_ex(ex: KindEx) -> Self {
        Self::new(KindSort::Ex(ex))
    }

    pub fn new_forall(var: KindVarId, body: Kind) -> Self {
        Self::new(KindSort::Forall(var, body))
    }

    //
    pub fn sort(&self) -> &KindSort {
        &self.data().sort
    }

    fn data(&self) -> &KindData {
        KIND_INTERNER.read().unwrap().expect(self.0)
    }

    //
    pub fn is_ty(&self) -> bool {
        matches!(self.sort(), KindSort::Ty(_))
    }

    pub fn as_ty(&self) -> Option<Ty> {
        match self.sort() {
            &KindSort::Ty(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn walk_inner_tys(&self) -> Box<dyn Iterator<Item = Ty> + '_> {
        match self.sort() {
            KindSort::Ty(ty) => ty.walk_inner_tys(),
            KindSort::Abs(param, body) => {
                Box::new(param.walk_inner_tys().chain(body.walk_inner_tys()))
            },
            KindSort::Var(_) | KindSort::Ex(_) => Box::new(std::iter::empty()),
            KindSort::Forall(_, body) => body.walk_inner_tys(),
        }
    }

    //
    pub fn substitute(&self, subst: KindVarId, with: Kind) -> Self {
        match self.sort() {
            KindSort::Ex(_) | KindSort::Ty(_) => *self,
            KindSort::Abs(param, body) => {
                Kind::new_abs(param.substitute(subst, with), body.substitute(subst, with))
            },
            &KindSort::Var(var) => {
                if subst == var {
                    with
                } else {
                    *self
                }
            },
            &KindSort::Forall(var, body) => {
                if subst == var {
                    Kind::new_forall(var, with)
                } else {
                    Kind::new_forall(var, body.substitute(subst, with))
                }
            },
        }
    }

    pub fn substitute_ty(&self, subst: TyVarId, with: Kind) -> Self {
        match self.sort() {
            KindSort::Ex(_) | &KindSort::Var(_) | KindSort::Ty(_) => *self,
            KindSort::Abs(param, body) => Kind::new_abs(
                param.substitute_ty(subst, with),
                body.substitute_ty(subst, with),
            ),
            &KindSort::Forall(var, body) => Kind::new_forall(var, body.substitute_ty(subst, with)),
        }
    }

    pub fn apply_ctx(&self, ctx: &impl AlgoCtx) -> Kind {
        match self.sort() {
            &KindSort::Ty(ty) => Kind::new_ty(ty.apply_ctx(ctx)),
            &KindSort::Var(_) => *self,
            &KindSort::Abs(param, body) => Kind::new_abs(param.apply_ctx(ctx), body.apply_ctx(ctx)),
            &KindSort::Ex(ex) => ctx
                .get_kind_ex_solution(ex)
                .map_or(*self, |kind| kind.apply_ctx(ctx)),
            &KindSort::Forall(var, body) => Kind::new_forall(var, body.apply_ctx(ctx)),
        }
    }

    pub fn contains_ex(&self, ex: KindEx) -> bool {
        match self.sort() {
            KindSort::Ty(_) => false,
            KindSort::Var(_) => false,
            KindSort::Abs(param, body) => param.contains_ex(ex) || body.contains_ex(ex),
            &KindSort::Ex(ex_) => ex == ex_,
            KindSort::Forall(_, body) => body.contains_ex(ex),
        }
    }

    pub fn contains_ty_ex(&self, ex: Ex) -> bool {
        match self.sort() {
            KindSort::Ty(ty) => ty.contains_ex(ex),
            KindSort::Abs(param, body) => param.contains_ty_ex(ex) | body.contains_ty_ex(ex),
            KindSort::Var(_) | KindSort::Ex(_) => false,
            KindSort::Forall(_, body) => body.contains_ty_ex(ex),
        }
    }

    /// This is a context-independent check, i.e. we applied context and
    ///  if existentials occur
    pub fn is_solved(&self) -> bool {
        match self.sort() {
            KindSort::Ty(ty) => ty.is_solved(),
            KindSort::Abs(param, body) => param.is_solved() && body.is_solved(),
            // TODO: Why true?
            KindSort::Var(_) => true,
            KindSort::Ex(_) => false,
            KindSort::Forall(_, body) => body.is_solved(),
        }
    }

    pub fn get_ty_vars(&self) -> HashSet<TyVarId> {
        match self.sort() {
            KindSort::Var(_) | KindSort::Ex(_) => HashSet::default(),
            KindSort::Ty(ty) => ty.inner_ty_vars(),
            KindSort::Abs(param, body) => param
                .get_ty_vars()
                .into_iter()
                .chain(body.get_ty_vars())
                .collect(),
            KindSort::Forall(_, kind) => kind.get_ty_vars(),
        }
    }
}

impl Into<Ty> for Kind {
    fn into(self) -> Ty {
        Ty::ty_kind(self)
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data())
    }
}

// Yes, KindKind, you see it, it is right here
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum KindSort {
    Ty(Ty),
    Abs(Kind, Kind),
    Var(KindVarId),
    Ex(KindEx),
    Forall(KindVarId, Kind),
}

impl Display for KindSort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KindSort::Ty(ty) => write!(f, "'({})", ty),
            KindSort::Abs(param, body) => write!(f, "{param} -> {body}"),
            KindSort::Var(var) => write!(f, "{}", var.pretty()),
            KindSort::Ex(ex) => write!(f, "{ex}"),
            KindSort::Forall(var, body) => write!(f, "forall {}. {body}", var.pretty()),
        }
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct KindData {
    sort: KindSort,
}

impl Display for KindData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.sort)
    }
}

mod interner {
    use std::{
        collections::{hash_map::DefaultHasher, HashMap},
        hash::{Hash, Hasher},
        sync::RwLock,
    };

    use once_cell::sync::Lazy;

    use super::{KindData, KindId, KindVarId};
    use crate::{
        dt::idx::{Idx, IndexVec},
        span::sym::Ident,
    };

    pub struct KindInterner {
        map: HashMap<u64, KindId>,
        kinds: Vec<&'static KindData>,
        kind_var_id: KindVarId,
        kind_var_names: IndexVec<KindVarId, Option<Ident>>,
    }

    impl KindInterner {
        pub fn new() -> Self {
            Self {
                map: Default::default(),
                kinds: Default::default(),
                kind_var_id: KindVarId::new(0),
                kind_var_names: Default::default(),
            }
        }

        fn hash(kind: &KindData) -> u64 {
            let mut state = DefaultHasher::new();
            kind.hash(&mut state);
            state.finish()
        }

        pub fn intern(&mut self, data: KindData) -> KindId {
            let hash = Self::hash(&data);

            if let Some(id) = self.map.get(&hash) {
                return *id;
            }

            // !Leaked
            let ty = Box::leak(Box::new(data));
            let id = KindId::from(self.kinds.len());

            self.map.insert(hash, id);
            self.kinds.push(ty);

            id
        }

        pub fn expect(&self, id: KindId) -> &'static KindData {
            self.kinds
                .get(id.as_usize())
                .expect(&format!("Failed to find type by type id {}", id))
        }

        pub fn next_kind_var_id(&mut self, name: Option<Ident>) -> KindVarId {
            let id = *self.kind_var_id.inc();
            if let Some(name) = name {
                assert!(self.kind_var_names.insert(id, name).is_none());
            }
            id
        }

        pub fn kind_var_name(&self, id: KindVarId) -> Option<Ident> {
            self.kind_var_names.get_flat(id).copied()
        }
    }

    pub static KIND_INTERNER: Lazy<RwLock<KindInterner>> =
        Lazy::new(|| RwLock::new(KindInterner::new()));
}
