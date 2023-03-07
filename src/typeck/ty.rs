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
    dt::idx::{declare_idx, Idx},
    hir::{self, expr::Lit},
};

declare_idx!(ExistentialId, u32, "^{}", Color::Blue);
declare_idx!(TyVarId, u32, "{}", Color::Cyan);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ExistentialKind {
    Common,
    Int,
    Float,
}

impl std::fmt::Display for ExistentialKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExistentialKind::Common => "",
            ExistentialKind::Int => "int",
            ExistentialKind::Float => "float",
        }
        .fmt(f)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Existential {
    kind: ExistentialKind,
    id: ExistentialId,
}

impl Existential {
    pub fn new(kind: ExistentialKind, id: ExistentialId) -> Self {
        Self { kind, id }
    }

    pub fn common(id: ExistentialId) -> Self {
        Self::new(ExistentialKind::Common, id)
    }

    pub fn int(id: ExistentialId) -> Self {
        Self::new(ExistentialKind::Int, id)
    }

    pub fn float(id: ExistentialId) -> Self {
        Self::new(ExistentialKind::Float, id)
    }

    pub fn id(&self) -> ExistentialId {
        self.id
    }

    pub fn kind(&self) -> ExistentialKind {
        self.kind
    }

    pub fn is_common(&self) -> bool {
        self.kind() == ExistentialKind::Common
    }

    pub fn is_int(&self) -> bool {
        self.kind() == ExistentialKind::Int
    }

    pub fn is_float(&self) -> bool {
        self.kind() == ExistentialKind::Float
    }
}

impl std::fmt::Display for Existential {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.kind(), self.id())
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
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
            IntKind::U8 | IntKind::I8 => 8,
            IntKind::U16 | IntKind::I16 => 16,
            IntKind::U32 | IntKind::I32 => 32,
            IntKind::I64 | IntKind::U64 => 64,
            // FIXME: Okay?
            IntKind::Uint | IntKind::Int => (std::mem::size_of::<*const u8>() * 8) as u8,
        }
    }

    pub fn bits(&self) -> u8 {
        self.bytes() * 8
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

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
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

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum PrimTy {
    Bool,
    Int(IntKind),
    Float(FloatKind),
    String,
}

impl TryFrom<Lit> for PrimTy {
    type Error = ();

    fn try_from(kind: Lit) -> Result<Self, Self::Error> {
        match kind {
            Lit::Bool(_) => Ok(Self::Bool),
            Lit::Int(_, kind) => IntKind::try_from(kind).map(|kind| PrimTy::Int(kind)),
            Lit::Float(_, kind) => FloatKind::try_from(kind).map(|kind| PrimTy::Float(kind)),
            Lit::String(_) => Ok(Self::String),
        }
    }
}

impl std::fmt::Display for PrimTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimTy::Bool => write!(f, "bool"),
            PrimTy::String => write!(f, "string"),
            PrimTy::Int(kind) => write!(f, "{}", kind),
            PrimTy::Float(kind) => write!(f, "{}", kind),
        }
    }
}

declare_idx!(TyId, u32, "#{}", Color::BrightYellow);

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Ty(TyId);

impl Ty {
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

    pub fn unit() -> Ty {
        Self::new(TyKind::Unit)
    }

    pub fn error() -> Ty {
        Self::new(TyKind::Error)
    }

    pub fn var(id: TyVarId) -> Ty {
        Self::new(TyKind::Var(id))
    }

    pub fn prim(prim: PrimTy) -> Ty {
        Self::new(TyKind::Prim(prim))
    }

    pub fn func(param: Ty, ret: Ty) -> Ty {
        Self::new(TyKind::Func(param, ret))
    }

    pub fn forall(var: TyVarId, body: Ty) -> Ty {
        Self::new(TyKind::Forall(var, body))
    }

    pub fn existential(ex: Existential) -> Ty {
        Self::new(TyKind::Existential(ex))
    }

    pub fn default_int() -> Ty {
        Self::prim(PrimTy::Int(DEFAULT_INT_KIND))
    }

    pub fn default_float() -> Ty {
        Self::prim(PrimTy::Float(DEFAULT_FLOAT_KIND))
    }

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

    pub fn is_mono(&self) -> bool {
        match self.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Prim(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => true,
            TyKind::Func(param, body) => param.is_mono() && body.is_mono(),
            TyKind::Forall(_, _) => false,
        }
    }

    pub fn is_instantiated(&self) -> bool {
        match self.kind() {
            TyKind::Error => todo!(),
            TyKind::Unit
            | TyKind::Prim(PrimTy::Bool | PrimTy::Float(_) | PrimTy::Int(_) | PrimTy::String) => {
                true
            },
            TyKind::Var(_) | TyKind::Existential(_) | TyKind::Forall(_, _) => false,
            &TyKind::Func(param, body) => param.is_instantiated() && body.is_instantiated(),
        }
    }

    pub fn substitute(&self, subst: Subst, with: Ty) -> Ty {
        verbose!("Substitute {} in {} with {}", subst, self, with);

        match self.kind() {
            TyKind::Error | TyKind::Unit | TyKind::Prim(_) => *self,
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
            &TyKind::Func(param_ty, return_ty) => {
                let param = param_ty.substitute(subst, with);
                let ret = return_ty.substitute(subst, with);
                Ty::func(param, ret)
            },
            &TyKind::Forall(ident, body) => {
                if subst == ident {
                    Ty::forall(ident, with)
                } else {
                    let subst = body.substitute(subst, with);
                    Ty::forall(ident, subst)
                }
            },
        }
    }

    // Strict getters //
    pub fn as_int_kind(&self) -> IntKind {
        match self.kind() {
            TyKind::Prim(prim) => match prim {
                &PrimTy::Int(kind) => kind,
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn as_float_kind(&self) -> FloatKind {
        match self.kind() {
            TyKind::Prim(prim) => match prim {
                &PrimTy::Float(kind) => kind,
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    pub fn as_func(&self) -> (Ty, Ty) {
        match self.kind() {
            &TyKind::Func(param, body) => (param, body),
            _ => panic!(),
        }
    }

    pub fn return_ty(&self) -> Ty {
        self.as_func().1
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
        match self.kind() {
            TyKind::Error => write!(f, "[ERROR]"),
            TyKind::Unit => write!(f, "()"),
            TyKind::Prim(lit) => write!(f, "{}", lit),
            TyKind::Var(name) => write!(f, "{}", name),
            TyKind::Existential(ex) => write!(f, "{}", ex),
            &TyKind::Func(param, body) => write!(f, "({} -> {})", param, body),
            &TyKind::Forall(alpha, ty) => write!(f, "(∀{}. {})", alpha, ty),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TyKind {
    Error,

    Unit,
    Prim(PrimTy),
    Var(TyVarId),
    Existential(Existential),
    Func(Ty, Ty),
    Forall(TyVarId, Ty),
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct TyS {
    kind: TyKind,
}

impl TyS {
    pub fn new(kind: TyKind) -> Self {
        Self { kind }
    }

    pub fn lit(lit_ty: PrimTy) -> Self {
        Self::new(TyKind::Prim(lit_ty))
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
        match self.kind() {
            TyKind::Error => write!(f, "[ERROR]"),
            TyKind::Unit => write!(f, "()"),
            TyKind::Prim(lit) => write!(f, "{}", lit),
            TyKind::Var(ident) => write!(f, "{}", ident),
            TyKind::Existential(ex) => write!(f, "{}", ex),
            TyKind::Func(param_ty, return_ty) => write!(f, "{} -> {}", param_ty, return_ty),
            TyKind::Forall(ident, ty) => write!(f, "∀{}. {}", ident, ty),
        }
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
