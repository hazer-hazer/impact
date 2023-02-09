use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
};

use crate::{
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, Idx},
    hir::{self, expr::Lit},
    span::span::Ident,
};

use super::ctx::{ExistentialId};

declare_idx!(TypeVarId, usize, "{}", Color::Green);

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

impl From<hir::expr::IntKind> for IntKind {
    fn from(kind: hir::expr::IntKind) -> Self {
        match kind {
            hir::expr::IntKind::Unknown => todo!(),
            hir::expr::IntKind::U8 => Self::U8,
            hir::expr::IntKind::U16 => Self::U16,
            hir::expr::IntKind::U32 => Self::U32,
            hir::expr::IntKind::U64 => Self::U64,
            hir::expr::IntKind::Uint => Self::Uint,
            hir::expr::IntKind::I8 => Self::I8,
            hir::expr::IntKind::I16 => Self::I16,
            hir::expr::IntKind::I32 => Self::I32,
            hir::expr::IntKind::I64 => Self::I64,
            hir::expr::IntKind::Int => Self::Int,
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

impl Display for IntKind {
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

impl From<hir::expr::FloatKind> for FloatKind {
    fn from(kind: hir::expr::FloatKind) -> Self {
        match kind {
            hir::expr::FloatKind::Unknown => todo!(),
            hir::expr::FloatKind::F32 => Self::F32,
            hir::expr::FloatKind::F64 => Self::F64,
        }
    }
}

pub const DEFAULT_FLOAT_KIND: FloatKind = FloatKind::F32;

impl Display for FloatKind {
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

impl From<Lit> for PrimTy {
    fn from(lit: Lit) -> Self {
        match lit {
            Lit::Bool(_) => Self::Bool,
            Lit::Int(_, kind) => Self::Int(IntKind::from(kind)),
            Lit::Float(_, kind) => Self::Float(FloatKind::from(kind)),
            Lit::String(_) => Self::String,
        }
    }
}

impl Display for PrimTy {
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

pub type Ty = TyId;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TyKind {
    Error,

    Unit,
    Lit(PrimTy),
    Var(Ident),
    Existential(ExistentialId),
    Func(Ty, Ty),
    Forall(Ident, Ty),
}

// pub enum MonoTy<'a> {
//     Error,
//     Unit,
//     Lit(PrimTy),
//     Var(Ident),
//     Existential(ExistentialId),
//     Func(&'a MonoTy<'a>, &'a MonoTy<'a>),
// }

// pub enum PolyTy<'a> {
//     Func(&'a MonoPolyTy<'a>, &'a MonoPolyTy<'a>),
//     Forall(Ident, &'a MonoPolyTy<'a>),
// }

// // I'm so proud of giving such names to structures
// pub enum MonoPolyTy<'a> {
//     Mono(MonoTy<'a>),
//     Poly(PolyTy<'a>),
// }

// impl<'a> MonoPolyTy<'a> {
//     pub fn is_mono(&self) -> bool {
//         match self {
//             MonoPolyTy::Mono(_) => true,
//             MonoPolyTy::Poly(_) => false,
//         }
//     }

//     pub fn is_poly(&self) -> bool {
//         !self.is_mono()
//     }

//     pub fn as_mono(&self) -> Option<&MonoTy> {
//         match self {
//             MonoPolyTy::Mono(mono) => Some(mono),
//             MonoPolyTy::Poly(_) => None,
//         }
//     }

//     pub fn as_poly(&self) -> Option<&PolyTy> {
//         match self {
//             MonoPolyTy::Mono(_) => None,
//             MonoPolyTy::Poly(poly) => Some(poly),
//         }
//     }
// }

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct TyS {
    kind: TyKind,
}

impl TyS {
    pub fn new(kind: TyKind) -> Self {
        Self { kind }
    }

    pub fn lit(lit_ty: PrimTy) -> Self {
        Self::new(TyKind::Lit(lit_ty))
    }

    pub fn error() -> Self {
        Self::new(TyKind::Error)
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }
}

impl Display for TyS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            TyKind::Error => write!(f, "[ERROR]"),
            TyKind::Unit => write!(f, "()"),
            TyKind::Lit(lit) => write!(f, "{}", lit),
            TyKind::Var(ident) => write!(f, "{}", ident),
            TyKind::Existential(ident) => write!(f, "{}^", ident),
            TyKind::Func(param_ty, return_ty) => write!(f, "{} -> {}", param_ty, return_ty),
            TyKind::Forall(ident, ty) => write!(f, "∀{}. {}", ident, ty),
        }
    }
}

pub struct TyError();
pub type TyResult<T> = Result<T, TyError>;

#[derive(Default)]
pub struct TyInterner {
    map: HashMap<u64, TyId>,
    types: Vec<TyS>,
}

impl TyInterner {
    fn hash(ty: &TyS) -> u64 {
        let mut state = DefaultHasher::new();
        ty.hash(&mut state);
        state.finish()
    }

    pub fn intern(&mut self, ty: TyS) -> TyId {
        let hash = Self::hash(&ty);

        if let Some(id) = self.map.get(&hash) {
            return *id;
        }

        let id = TyId::from(self.types.len());

        self.map.insert(hash, id);
        self.types.push(ty);

        id
    }

    pub fn expect(&self, ty: TyId) -> &TyS {
        self.types.get(ty.as_usize()).unwrap()
    }
}

#[derive(Clone, Copy)]
pub enum Subst {
    Existential(ExistentialId),
    Name(Ident),
}

impl PartialEq<Ident> for Subst {
    fn eq(&self, other: &Ident) -> bool {
        match (self, other) {
            (Self::Name(name), other) => name == other,
            _ => false,
        }
    }
}

impl PartialEq<ExistentialId> for Subst {
    fn eq(&self, other: &ExistentialId) -> bool {
        match (self, other) {
            (Self::Existential(ex), other) => ex == other,
            _ => false,
        }
    }
}
