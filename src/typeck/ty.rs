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

use super::ctx::ExistentialId;

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
    IntEx(ExistentialId),
    Int(IntKind),
    FloatEx(ExistentialId),
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

impl Display for PrimTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimTy::Bool => write!(f, "bool"),
            PrimTy::String => write!(f, "string"),
            PrimTy::Int(kind) => write!(f, "{}", kind),
            PrimTy::Float(kind) => write!(f, "{}", kind),
            PrimTy::IntEx(ex) => write!(f, "int^{}", ex),
            PrimTy::FloatEx(ex) => write!(f, "float^{}", ex),
        }
    }
}

declare_idx!(TyId, u32, "#{}", Color::BrightYellow);

pub type Ty = TyId;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TyKind {
    Error,

    Unit,
    Prim(PrimTy),
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
        Self::new(TyKind::Prim(lit_ty))
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
            TyKind::Prim(lit) => write!(f, "{}", lit),
            TyKind::Var(ident) => write!(f, "{}", ident),
            TyKind::Existential(ident) => write!(f, "{}", ident),
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

#[derive(Clone, Copy, Debug)]
pub enum Subst {
    Existential(ExistentialId),
    Name(Ident),
}

impl Display for Subst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Subst::Existential(ex) => ex.fmt(f),
            Subst::Name(name) => name.fmt(f),
        }
    }
}

impl PartialEq<Ident> for Subst {
    fn eq(&self, other: &Ident) -> bool {
        match (self, other) {
            (Self::Name(name), other) => name.sym() == other.sym(),
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

#[cfg(test)]
mod tests {
    use crate::{
        session::SourceId,
        span::span::{Ident, Internable, Span},
        typeck::ctx::ExistentialId,
    };

    use super::Subst;

    const SOME_SOURCE_ID: SourceId = SourceId::new(123);

    #[test]
    fn same_ident_subst_eq() {
        let name = Ident::synthetic("lolkek".intern());
        assert_eq!(Subst::Name(name), name);
    }

    #[test]
    fn diff_idents_subst_eq() {
        assert_eq!(
            Subst::Name(Ident::new(Span::new(0, 1, SOME_SOURCE_ID), "a".intern())),
            Ident::new(Span::new(123, 10, SOME_SOURCE_ID), "a".intern())
        )
    }

    #[test]
    fn diff_idents_subst_ne() {
        assert_ne!(
            Subst::Name(Ident::new(Span::new(0, 1, SOME_SOURCE_ID), "a".intern())),
            Ident::new(Span::new(123, 10, SOME_SOURCE_ID), "b".intern())
        )
    }

    #[test]
    fn same_ex_subst_eq() {
        let ex = ExistentialId::new(1);
        assert_eq!(Subst::Existential(ex), ex)
    }

    #[test]
    fn diff_exes_subst_eq() {
        assert_eq!(
            Subst::Existential(ExistentialId::new(1)),
            ExistentialId::new(1)
        )
    }

    #[test]
    fn diff_exes_subst_ne() {
        assert_ne!(
            Subst::Existential(ExistentialId::new(1)),
            ExistentialId::new(2)
        )
    }
}
