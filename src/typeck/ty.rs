use once_cell::sync::Lazy;

use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::Formatter,
    hash::{Hash, Hasher},
    sync::RwLock,
};

use crate::{
    cli::{
        color::{Color, Colorize},
        verbose,
    },
    dt::idx::{declare_idx, Idx, IndexVec},
    hir::{self},
    resolve::def::DefId,
    span::span::{Ident, Symbol},
    utils::macros::match_expected,
};

use super::{
    ctx::AlgoCtx,
    kind::{Kind, KindEx, KindSort},
};

declare_idx!(TyId, u32, "#{}", Color::BrightYellow);
declare_idx!(ExistentialId, u32, "^{}", Color::Blue);
declare_idx!(TyVarId, u32, "{}", Color::Cyan);
declare_idx!(VariantIdx, u32, "{}", Color::White);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ExKind {
    Common,
    Int,
    Float,
}

impl std::fmt::Display for ExKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExKind::Common => "",
            ExKind::Int => "int",
            ExKind::Float => "float",
        }
        .fmt(f)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Existential {
    kind: ExKind,
    id: ExistentialId,
}

/// Frequently used pair of Existential with Ty for this existential.
pub type ExPair<E = Existential> = (E, Ty);

impl Existential {
    pub fn new(kind: ExKind, id: ExistentialId) -> Self {
        Self { kind, id }
    }

    pub fn common(id: ExistentialId) -> Self {
        Self::new(ExKind::Common, id)
    }

    pub fn int(id: ExistentialId) -> Self {
        Self::new(ExKind::Int, id)
    }

    pub fn float(id: ExistentialId) -> Self {
        Self::new(ExKind::Float, id)
    }

    pub fn id(&self) -> ExistentialId {
        self.id
    }

    pub fn kind(&self) -> ExKind {
        self.kind
    }

    pub fn is_common(&self) -> bool {
        self.kind() == ExKind::Common
    }

    pub fn is_int(&self) -> bool {
        self.kind() == ExKind::Int
    }

    pub fn is_float(&self) -> bool {
        self.kind() == ExKind::Float
    }
}

impl std::fmt::Display for Existential {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.kind(), self.id())
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum IntKind {
    U8,
    U16,
    U32,
    U64,
    Uint,

    I8,
    I16,
    I32,
    I64,
    Int,
}

impl IntKind {
    pub fn bytes(&self) -> u8 {
        match self {
            IntKind::U8 | IntKind::I8 => 1,
            IntKind::U16 | IntKind::I16 => 2,
            IntKind::U32 | IntKind::I32 => 4,
            IntKind::I64 | IntKind::U64 => 8,
            // FIXME: Okay?
            IntKind::Uint | IntKind::Int => (std::mem::size_of::<*const u8>() * 8) as u8,
        }
    }

    pub fn bits(&self) -> u32 {
        self.bytes() as u32 * 8
    }
}

impl TryFrom<hir::expr::IntKind> for IntKind {
    type Error = ();

    fn try_from(kind: hir::expr::IntKind) -> Result<Self, Self::Error> {
        match kind {
            hir::expr::IntKind::Unknown => Err(()),
            hir::expr::IntKind::U8 => Ok(Self::U8),
            hir::expr::IntKind::U16 => Ok(Self::U16),
            hir::expr::IntKind::U32 => Ok(Self::U32),
            hir::expr::IntKind::U64 => Ok(Self::U64),
            hir::expr::IntKind::Uint => Ok(Self::Uint),
            hir::expr::IntKind::I8 => Ok(Self::I8),
            hir::expr::IntKind::I16 => Ok(Self::I16),
            hir::expr::IntKind::I32 => Ok(Self::I32),
            hir::expr::IntKind::I64 => Ok(Self::I64),
            hir::expr::IntKind::Int => Ok(Self::Int),
        }
    }
}

pub const DEFAULT_INT_KIND: IntKind = IntKind::I32;

impl IntKind {
    pub fn is_unsigned(&self) -> bool {
        match self {
            IntKind::U8 | IntKind::U16 | IntKind::U32 | IntKind::U64 | IntKind::Uint => true,
            IntKind::I8 | IntKind::I16 | IntKind::I32 | IntKind::I64 | IntKind::Int => false,
        }
    }
}

impl std::fmt::Display for IntKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntKind::U8 => "u8",
                IntKind::U16 => "u16",
                IntKind::U32 => "u32",
                IntKind::U64 => "u64",
                IntKind::Uint => "uint",
                IntKind::I8 => "i8",
                IntKind::I16 => "i16",
                IntKind::I32 => "i32",
                IntKind::I64 => "i64",
                IntKind::Int => "int",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum FloatKind {
    F32,
    F64,
}

impl FloatKind {
    pub fn bytes(&self) -> u8 {
        match self {
            FloatKind::F32 => 4,
            FloatKind::F64 => 8,
        }
    }
}

impl TryFrom<hir::expr::FloatKind> for FloatKind {
    type Error = ();

    fn try_from(kind: hir::expr::FloatKind) -> Result<Self, Self::Error> {
        match kind {
            hir::expr::FloatKind::Unknown => Err(()),
            hir::expr::FloatKind::F32 => Ok(Self::F32),
            hir::expr::FloatKind::F64 => Ok(Self::F64),
        }
    }
}

pub const DEFAULT_FLOAT_KIND: FloatKind = FloatKind::F32;

impl std::fmt::Display for FloatKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FloatKind::F32 => "f32",
                FloatKind::F64 => "f64",
            }
        )
    }
}

pub type TyMap<T> = IndexVec<TyId, Option<T>>;

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Ty(TyId);

impl Ty {
    pub fn id(&self) -> TyId {
        self.0
    }

    // Interning and constructors //
    pub fn intern(tys: TyS) -> Self {
        Self(TY_INTERNER.write().unwrap().intern(tys))
    }

    fn new(kind: TyKind) -> Self {
        Self::intern(TyS::new(kind))
    }

    pub fn next_ty_var_id() -> TyVarId {
        *TY_INTERNER.write().unwrap().ty_var_id.inc()
    }

    // Constructors //
    pub fn unit() -> Ty {
        Self::new(TyKind::Unit)
    }

    pub fn error() -> Ty {
        Self::new(TyKind::Error)
    }

    pub fn var(id: TyVarId) -> Ty {
        Self::new(TyKind::Var(id))
    }

    pub fn bool() -> Ty {
        Self::new(TyKind::Bool)
    }

    pub fn int(kind: IntKind) -> Ty {
        Self::new(TyKind::Int(kind))
    }

    pub fn float(kind: FloatKind) -> Ty {
        Self::new(TyKind::Float(kind))
    }

    pub fn str() -> Ty {
        Self::new(TyKind::Str)
    }

    pub fn func(def_id: Option<DefId>, params: Vec<Ty>, body: Ty) -> Ty {
        assert!(!params.is_empty());
        Self::new(if let Some(def_id) = def_id {
            TyKind::FuncDef(def_id, params, body)
        } else {
            TyKind::Func(params, body)
        })
    }

    pub fn ref_to(ty: Ty) -> Ty {
        // assert!(ty.is_mono());
        Self::new(TyKind::Ref(ty))
    }

    pub fn forall(var: TyVarId, body: Ty) -> Ty {
        Self::new(TyKind::Forall(var, body))
    }

    pub fn existential(ex: Existential) -> Ty {
        Self::new(TyKind::Existential(ex))
    }

    pub fn ty_kind(kind: Kind) -> Ty {
        Self::new(TyKind::Kind(kind))
    }

    pub fn default_int() -> Ty {
        Self::int(DEFAULT_INT_KIND)
    }

    pub fn default_float() -> Ty {
        Self::float(DEFAULT_FLOAT_KIND)
    }

    //
    pub fn tys(&self) -> &TyS {
        TY_INTERNER.read().unwrap().expect(self.0)
    }

    pub fn kind(&self) -> &TyKind {
        self.tys().kind()
    }

    pub fn as_ex(&self) -> Option<Existential> {
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

    pub fn is_ty(&self) -> bool {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::FuncDef(_, _, _)
            | TyKind::Func(_, _)
            | TyKind::Ref(_)
            // TODO: Can existentials and vars be higher-kinded?
            | TyKind::Var(_)
            | TyKind::Existential(_)
            | TyKind::Forall(_, _) => true,
            TyKind::Kind(_) => false,
        }
    }

    pub fn is_kind(&self) -> bool {
        matches!(self.kind(), TyKind::Kind(_))
    }

    pub fn assert_is_ty(&self) {
        assert!(self.is_ty(), "Expected a type, got {}", self);
    }

    pub fn is_mono(&self) -> bool {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Var(_)
            | TyKind::Existential(_) => true,
            TyKind::FuncDef(_, params, body) | TyKind::Func(params, body) => {
                params.iter().all(Ty::is_mono) && body.is_mono()
            },
            TyKind::Forall(_, _) => false,
            TyKind::Ref(ty) => ty.is_mono(),
            TyKind::Kind(_) => false,
        }
    }

    pub fn is_func_like(&self) -> bool {
        matches!(self.kind(), TyKind::Func(..) | TyKind::FuncDef(..))
    }

    pub fn is_instantiated(&self) -> bool {
        match self.kind() {
            TyKind::Error => todo!(),
            TyKind::Unit | TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) | TyKind::Str => true,
            TyKind::Var(_) | TyKind::Existential(_) | TyKind::Forall(_, _) => false,
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                params.iter().all(Ty::is_instantiated) && body.is_instantiated()
            },
            TyKind::Ref(ty) => ty.is_instantiated(),
            TyKind::Kind(_) => false,
        }
    }

    pub fn is_solved(&self) -> bool {
        match self.kind() {
            TyKind::Error => true,
            TyKind::Unit | TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) | TyKind::Str => true,
            TyKind::FuncDef(_, params, body) | TyKind::Func(params, body) => {
                params.iter().all(Ty::is_solved) && body.is_solved()
            },
            // TODO: Check that var is bound in ty_bindings?
            TyKind::Var(_) => true,
            TyKind::Existential(_) => false,
            TyKind::Forall(_, ty) => ty.is_solved(),
            TyKind::Ref(ty) => ty.is_solved(),
            TyKind::Kind(kind) => kind.is_solved(),
        }
    }

    pub fn as_mono(&self) -> Option<MonoTy> {
        let sort = match self.kind() {
            TyKind::Error => Some(MonoTyKind::Error),
            TyKind::Unit => Some(MonoTyKind::Unit),
            TyKind::Bool => Some(MonoTyKind::Bool),
            &TyKind::Int(kind) => Some(MonoTyKind::Int(kind)),
            &TyKind::Float(kind) => Some(MonoTyKind::Float(kind)),
            TyKind::Str => Some(MonoTyKind::String),
            TyKind::Ref(ty) => Some(MonoTyKind::Ref(Box::new(ty.as_mono().unwrap()))),
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                let params = params
                    .iter()
                    .map(|param| param.as_mono().ok_or(()))
                    .collect::<Result<Vec<_>, _>>()
                    .ok()?
                    .into_iter()
                    .map(Box::new)
                    .collect();
                let body = body.as_mono()?;
                Some(MonoTyKind::Func(params, Box::new(body)))
            },
            TyKind::Var(_) | TyKind::Existential(_) | TyKind::Forall(_, _) => None,
            TyKind::Kind(_) => None,
        }?;

        Some(MonoTy { sort, ty: *self })
    }

    pub fn contains_ex(&self, ex: Existential) -> bool {
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
        }
    }

    pub fn mono(&self) -> MonoTy {
        self.as_mono()
            .expect(&format!("{} expected to be a mono type", self))
    }

    pub fn mono_checked(self) -> Self {
        assert!(self.is_mono());
        self
    }

    pub fn func_def_id(&self) -> Option<DefId> {
        if let &TyKind::FuncDef(def_id, ..) = self.kind() {
            Some(def_id)
        } else {
            None
        }
    }

    pub fn maybe_add_func_def_id(&self, def_id: DefId) -> Ty {
        match self.kind() {
            TyKind::FuncDef(_, _, _) => panic!("Function DefId cannot be set type FuncDef"),
            TyKind::Func(params, body) => Ty::func(Some(def_id), params.clone(), *body),
            _ => *self,
        }
    }

    pub fn substitute(&self, subst: Subst, with: Ty) -> Ty {
        verbose!("Substitute {} in {} with {}", subst, self, with);

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
            &TyKind::Existential(ex) => {
                if subst == ex {
                    with
                } else {
                    *self
                }
            },
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
            TyKind::Kind(_) => todo!(),
        }
    }

    pub fn apply_ctx(&self, ctx: &impl AlgoCtx) -> Ty {
        let ty = self._apply_ctx(ctx);
        // verbose!("Apply ctx on {} => {}", self, ty);
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

            // FIXME: Can we panic on unwrap?
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
        }
    }

    // Strict getters //
    pub fn as_int(&self) -> IntKind {
        match_expected!(@pp self.kind(), &TyKind::Int(kind) => kind)
    }

    pub fn as_float_kind(&self) -> FloatKind {
        match_expected!(@pp self.kind(), &TyKind::Float(kind) => kind)
    }

    pub fn as_func(&self) -> (DefId, &[Ty], Ty) {
        match_expected!(self.kind(), TyKind::FuncDef(def_id, params, body) => (*def_id, params.as_ref(), *body))
    }

    pub fn as_func_like(&self) -> (&[Ty], Ty) {
        match_expected!(self.kind(), TyKind::FuncDef(_, params, body) | TyKind::Func(params, body) => (params.as_ref(), *body))
    }

    pub fn return_ty(&self) -> Ty {
        self.as_func_like().1
    }

    pub fn body_return_ty(&self) -> Ty {
        match self.kind() {
            &TyKind::FuncDef(_, _, body) | &TyKind::Func(_, body) => body,
            _ => *self,
        }
    }

    pub fn is_unit(&self) -> bool {
        self.kind() == &TyKind::Unit
    }
}

impl std::fmt::Debug for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ty({})", self)
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

pub type FuncTy = (Ty, Ty);

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Field {
    pub name: Option<Ident>,
    pub ty: Ty,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Variant {
    pub def_id: DefId,
    pub name: Ident,
    pub fields: Vec<Field>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TyKind {
    Error,

    Unit,
    Bool,
    Int(IntKind),
    Float(FloatKind),

    Str,

    /// Type of function or lambda
    FuncDef(DefId, Vec<Ty>, Ty),

    Func(Vec<Ty>, Ty),

    Data {
        def_id: DefId,
        variants: IndexVec<VariantIdx, Variant>,
    },
    Ref(Ty),

    Var(TyVarId),
    Existential(Existential),
    Forall(TyVarId, Ty),

    /// Type kind.
    /// This might seem strange, but `Kind` is inside `TyKind` to simplify typeck code,
    /// as we always work with `Ty`
    Kind(Kind),
}

impl std::fmt::Display for TyKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TyKind::Error => write!(f, "[ERROR]"),
            TyKind::Unit => write!(f, "()"),
            TyKind::Bool => write!(f, "bool"),
            TyKind::Int(kind) => write!(f, "{}", kind),
            TyKind::Float(kind) => write!(f, "{}", kind),
            TyKind::Str => write!(f, "{}", "string"),
            TyKind::Func(params, body) => write!(
                f,
                "({} -> {})",
                params
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" - "),
                body
            ),
            TyKind::FuncDef(def_id, params, body) => {
                write!(
                    f,
                    "({} -> {}){}",
                    params
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" "),
                    body,
                    def_id
                )
            },
            TyKind::Ref(ty) => write!(f, "ref {ty}"),
            TyKind::Var(name) => write!(f, "{name}"),
            TyKind::Existential(ex) => write!(f, "{ex}"),
            TyKind::Forall(alpha, ty) => write!(f, "(∀{alpha}. {ty})"),
            TyKind::Kind(kind) => write!(f, "{kind}"),
        }
    }
}

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

static TY_INTERNER: Lazy<RwLock<TyInterner>> = Lazy::new(|| RwLock::new(TyInterner::new()));

pub struct TyInterner {
    map: HashMap<u64, TyId>,
    types: Vec<&'static TyS>,
    ty_var_id: TyVarId,
}

impl TyInterner {
    pub fn new() -> Self {
        Self {
            map: Default::default(),
            types: Default::default(),
            ty_var_id: TyVarId(0),
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
}

#[derive(Clone, Copy, Debug)]
pub enum Subst {
    Existential(Existential),
    Var(TyVarId),
}

impl std::fmt::Display for Subst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Subst::Existential(ex) => ex.fmt(f),
            Subst::Var(var) => var.fmt(f),
        }
    }
}

impl PartialEq<TyVarId> for Subst {
    fn eq(&self, other: &TyVarId) -> bool {
        match (self, other) {
            (Self::Var(var), other) => var == other,
            _ => false,
        }
    }
}

impl PartialEq<Existential> for Subst {
    fn eq(&self, other: &Existential) -> bool {
        match (self, other) {
            (Self::Existential(ex), other) => ex.id() == other.id(),
            _ => false,
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
    Func(Vec<Box<MonoTy>>, Box<MonoTy>),
    Ref(Box<MonoTy>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MonoTy {
    pub sort: MonoTyKind,
    pub ty: Ty,
}

#[cfg(test)]
mod tests {
    use crate::session::SourceId;

    const SOME_SOURCE_ID: SourceId = SourceId::new(123);

    // #[test]
    // fn same_ident_subst_eq() {
    //     let name = Ident::synthetic("lolkek".intern());
    //     assert_eq!(Subst::Var(name), name);
    // }

    // #[test]
    // fn diff_idents_subst_eq() {
    //     assert_eq!(
    //         Subst::Var(Ident::new(Span::new(0, 1, SOME_SOURCE_ID), "a".intern())),
    //         Ident::new(Span::new(123, 10, SOME_SOURCE_ID), "a".intern())
    //     )
    // }

    // #[test]
    // fn diff_idents_subst_ne() {
    //     assert_ne!(
    //         Subst::Var(Ident::new(Span::new(0, 1, SOME_SOURCE_ID), "a".intern())),
    //         Ident::new(Span::new(123, 10, SOME_SOURCE_ID), "b".intern())
    //     )
    // }

    // #[test]
    // fn same_ex_subst_eq() {
    //     let ex = ExistentialId::new(1);
    //     assert_eq!(Subst::Existential(ex), ex)
    // }

    // #[test]
    // fn diff_exes_subst_eq() {
    //     assert_eq!(
    //         Subst::Existential(ExistentialId::new(1)),
    //         ExistentialId::new(1)
    //     )
    // }

    // #[test]
    // fn diff_exes_subst_ne() {
    //     assert_ne!(
    //         Subst::Existential(ExistentialId::new(1)),
    //         ExistentialId::new(2)
    //     )
    // }
}
