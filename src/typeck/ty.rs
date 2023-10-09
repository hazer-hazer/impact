use std::{
    fmt::Formatter,
    hash::{Hash, Hasher},
};

use super::kind::Kind;
use crate::{
    cli::color::{Color, WithColor},
    dt::idx::{declare_idx, Idx, IndexVec},
    hir::{self},
    resolve::def::DefId,
    session::{MaybeWithSession, WithSess, WithoutSess},
    span::sym::Ident,
    utils::macros::{match_opt, match_result},
};

declare_idx!(TyId, u32, "#{}", Color::BrightYellow);
declare_idx!(ExId, u32, "^{}", Color::Blue);
declare_idx!(TyVarId, u32, "{}", Color::Cyan);
declare_idx!(VariantId, u32, "{}", Color::White);

impl TyVarId {
    pub fn real_name(&self) -> String {
        Ty::ty_var_name_or_id(*self)
    }
}

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
pub struct Ex {
    kind: ExKind,
    id: ExId,
}

/// Frequently used pair of Existential with Ty for this existential.
pub type ExPair<E = Ex> = (E, Ty);

impl Ex {
    pub fn new(kind: ExKind, id: ExId) -> Self {
        Self { kind, id }
    }

    pub fn common(id: ExId) -> Self {
        Self::new(ExKind::Common, id)
    }

    pub fn int(id: ExId) -> Self {
        Self::new(ExKind::Int, id)
    }

    pub fn float(id: ExId) -> Self {
        Self::new(ExKind::Float, id)
    }

    pub fn id(&self) -> ExId {
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

impl std::fmt::Display for Ex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.kind(), self.id())
    }
}

impl WithColor for Ex {
    fn fg_color() -> Option<Color> {
        Some(Color::Blue)
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

pub type FuncTy = (Ty, Ty);

declare_idx!(FieldId, u32, "{}", Color::White);

/// The trait which is used in complex type structures, like ADT.
// No ~~bitches~~ HKT
pub trait MapTy<To, S> {
    fn map_ty<E, F>(&self, f: &mut F) -> Result<S, E>
    where
        F: FnMut(Ty) -> Result<To, E>;

    fn map_ty_pure<F>(&self, f: &mut F) -> S
    where
        F: FnMut(Ty) -> Result<To, ()>,
    {
        self.map_ty(f).unwrap()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Field<T = Ty> {
    pub name: Ident,
    pub ty: T,
}

impl<To> MapTy<To, Field<To>> for Field {
    fn map_ty<E, F>(&self, f: &mut F) -> Result<Field<To>, E>
    where
        F: FnMut(Ty) -> Result<To, E>,
    {
        let ty = f(self.ty)?;

        Ok(Field {
            name: self.name,
            ty,
        })
    }
}

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Variant<T = Ty> {
    pub def_id: DefId,
    pub name: Ident,
    pub fields: IndexVec<FieldId, Field<T>>,
}

impl Variant<Ty> {
    pub fn size(&self) -> Option<u32> {
        self.fields.iter().map(|f| f.ty.size()).sum()
    }
}

impl<To> MapTy<To, Variant<To>> for Variant {
    fn map_ty<E, F>(&self, f: &mut F) -> Result<Variant<To>, E>
    where
        F: FnMut(Ty) -> Result<To, E>,
    {
        let fields = self
            .fields
            .iter()
            .map(|field| field.map_ty(f))
            .collect::<Result<_, _>>()?;

        Ok(Variant {
            def_id: self.def_id,
            name: self.name,
            fields,
        })
    }
}

impl std::fmt::Display for Variant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            self.name,
            self.fields
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Adt<T = Ty> {
    pub def_id: DefId,
    pub variants: IndexVec<VariantId, Variant<T>>,
}

impl Adt {
    pub fn walk_tys<'a>(&'a self) -> impl Iterator<Item = Ty> + 'a {
        self.variants
            .iter()
            .map(|v| v.fields.iter().map(|f| f.ty))
            .flatten()
    }

    pub fn variant(&self, vid: VariantId) -> &Variant {
        self.variants.get(vid).unwrap()
    }

    pub fn field_ty(&self, vid: VariantId, fid: FieldId) -> Ty {
        self.variants.get(vid).unwrap().fields.get(fid).unwrap().ty
    }

    pub fn max_variant_size(&self) -> Option<u32> {
        self.variants
            .iter()
            .map(|v| v.size())
            .collect::<Option<Vec<_>>>()?
            .iter()
            .copied()
            .max()
    }
}

impl<To> MapTy<To, Adt<To>> for Adt {
    fn map_ty<E, F>(&self, f: &mut F) -> Result<Adt<To>, E>
    where
        F: FnMut(Ty) -> Result<To, E>,
    {
        let variants = self
            .variants
            .iter()
            .map(|variant| variant.map_ty(f))
            .collect::<Result<_, _>>()?;

        Ok(Adt {
            def_id: self.def_id,
            variants,
        })
    }
}

impl std::fmt::Display for Adt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "data {} = {}",
            self.def_id,
            self.variants
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" | ")
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Struct<T = Ty> {
    pub def_id: DefId,
    pub fields: IndexVec<FieldId, Field<T>>,
}

impl Struct {
    pub fn walk_tys<'a>(&'a self) -> impl Iterator<Item = Ty> + 'a {
        self.fields.iter().map(|f| f.ty)
    }

    pub fn size(&self) -> Option<u32> {
        self.fields.iter().map(|f| f.ty.size()).sum()
    }
}

impl<To> MapTy<To, Struct<To>> for Struct {
    fn map_ty<E, F>(&self, f: &mut F) -> Result<Struct<To>, E>
    where
        F: FnMut(Ty) -> Result<To, E>,
    {
        let fields = self
            .fields
            .iter()
            .map(|field| field.map_ty(f))
            .collect::<Result<_, _>>()?;

        Ok(Struct {
            def_id: self.def_id,
            fields,
        })
    }
}

impl std::fmt::Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "struct {} = {}",
            self.def_id,
            self.fields
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

pub type TypeAsResult<T> = Result<T, String>;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TyKind {
    Error,

    Unit,
    Bool,
    Int(IntKind),
    Float(FloatKind),

    Str,

    /// Type of function or lambda
    /// Note: Sync with `FuncDefTyInner`
    FuncDef(DefId, Vec<Ty>, Ty),

    /// Note: Sync with `FuncTyInner`
    Func(Vec<Ty>, Ty),

    Adt(Adt),
    Struct(Struct),
    Ref(Ty),

    Var(TyVarId),
    Existential(Ex),
    Forall(TyVarId, Ty),

    /// Type kind.
    /// This might seem strange, but `Kind` is inside `TyKind` to simplify
    /// typeck code, as we always work with `Ty`
    Kind(Kind),
}

impl std::fmt::Display for TyKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TyKind::Error => write!(f, "[ERROR]"),
            TyKind::Unit => write!(f, "()"),
            TyKind::Bool => write!(f, "bool"),
            TyKind::Int(kind) => write!(f, "{kind}"),
            TyKind::Float(kind) => write!(f, "{kind}"),
            TyKind::Str => write!(f, "{}", "string"),
            TyKind::Func(params, body) => write!(
                f,
                "({} -> {})",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(" - "),
                body
            ),
            TyKind::FuncDef(_def_id, params, body) => {
                write!(
                    f,
                    "{} -> {}",
                    params
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                    body,
                    // def_id
                )
            },
            TyKind::Adt(adt) => adt.fmt(f),
            TyKind::Struct(data) => data.fmt(f),
            TyKind::Ref(ty) => write!(f, "ref {}", ty),
            &TyKind::Var(id) => write!(f, "{}", id.real_name()),
            TyKind::Existential(ex) => write!(f, "{ex}"),
            TyKind::Forall(alpha, ty) => {
                write!(f, "(∀{}. {})", alpha.real_name(), ty)
            },
            TyKind::Kind(kind) => write!(f, "{kind}"),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Ty(pub(super) TyId);

// TODO: Split Ty methods into separate files for Typeck and public usage.
impl Ty {
    pub fn id(&self) -> TyId {
        self.0
    }

    pub fn kind(&self) -> &TyKind {
        self.tys().kind()
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

    /// Generate function if there're more than one parameter, otherwise type is
    /// the body type
    pub fn tight_func(def_id: Option<DefId>, params: Vec<Ty>, body: Ty) -> Ty {
        if params.is_empty() {
            body
        } else {
            Self::func(def_id, params, body)
        }
    }

    pub fn adt(adt: Adt) -> Ty {
        Self::new(TyKind::Adt(adt))
    }

    pub fn struct_(struct_: Struct) -> Ty {
        Self::new(TyKind::Struct(struct_))
    }

    pub fn ref_to(ty: Ty) -> Ty {
        // assert!(ty.is_mono());
        Self::new(TyKind::Ref(ty))
    }

    pub fn forall(var: TyVarId, body: Ty) -> Ty {
        Self::new(TyKind::Forall(var, body))
    }

    pub fn ex(ex: Ex) -> Ty {
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

    // Checks //
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
            TyKind::Struct(s) => s.walk_tys().all(|ty| ty.is_mono()),
            TyKind::Adt(adt) => adt.walk_tys().all(|ty| ty.is_mono()),
            TyKind::Forall(..) => false,
            TyKind::Ref(ty) => ty.is_mono(),
            TyKind::Kind(_) => false,
        }
    }

    pub fn is_func_like(&self) -> bool {
        matches!(self.kind(), TyKind::Func(..) | TyKind::FuncDef(..))
    }

    pub fn is_adt(&self) -> bool {
        matches!(self.kind(), TyKind::Adt(..))
    }

    pub fn is_unit(&self) -> bool {
        self.kind() == &TyKind::Unit
    }

    pub fn is_instantiated(&self) -> bool {
        match self.kind() {
            TyKind::Error => todo!(),
            TyKind::Unit | TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) | TyKind::Str => true,
            TyKind::Var(_) | TyKind::Existential(_) | TyKind::Forall(..) => false,
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                params.iter().all(Ty::is_instantiated) && body.is_instantiated()
            },
            TyKind::Struct(s) => s.walk_tys().all(|ty| ty.is_instantiated()),
            TyKind::Adt(adt) => adt.walk_tys().all(|ty| ty.is_instantiated()),
            TyKind::Ref(ty) => ty.is_instantiated(),
            TyKind::Kind(_) => false,
        }
    }

    // Getters //
    pub fn as_adt(&self) -> TypeAsResult<&Adt> {
        match_result!(self.kind(), TyKind::Adt(adt) => adt)
    }

    pub fn as_struct(&self) -> TypeAsResult<&Struct> {
        match_result!(self.kind(), TyKind::Struct(struct_) => struct_)
    }

    pub fn as_int(&self) -> TypeAsResult<IntKind> {
        match_result!(self.kind(), &TyKind::Int(kind) => kind)
    }

    pub fn as_float_kind(&self) -> TypeAsResult<FloatKind> {
        match_result!(self.kind(), &TyKind::Float(kind) => kind)
    }

    // FIXME: Unused
    pub fn as_func_def(&self) -> Option<(DefId, &[Ty], Ty)> {
        match_opt!(self.kind(), TyKind::FuncDef(def_id, params, body) => (*def_id, params.as_ref(), *body))
    }

    pub fn as_func_like(&self) -> Option<(&[Ty], Ty)> {
        match_opt!(self.kind(), TyKind::FuncDef(_, params, body) | TyKind::Func(params, body) => (params.as_ref(), *body))
    }

    pub fn func_def_id(&self) -> Option<DefId> {
        if let &TyKind::FuncDef(def_id, ..) = self.kind() {
            Some(def_id)
        } else {
            None
        }
    }

    pub fn return_ty(&self) -> Option<Ty> {
        self.as_func_like().map(|func| func.1)
    }

    pub fn body_return_ty(&self) -> Ty {
        match self.kind() {
            &TyKind::FuncDef(_, _, body) | &TyKind::Func(_, body) => body,
            _ => *self,
        }
    }

    #[deprecated]
    // FIXME
    /// Deprecated because if `forall a. a` occurs, it adds one more `forall a.
    /// forall a.`. For each type variable, convert type to `forall var. ty`
    pub fn generalize(&self) -> Ty {
        self.inner_ty_vars()
            .into_iter()
            .fold(*self, |ty, var| Ty::forall(var, ty))
    }

    /// Get size of type in bytes, None if size is unknown at compile-time
    pub fn size(self) -> Option<u32> {
        match self.kind() {
            TyKind::Error => panic!(),
            // TODO: Review
            TyKind::Unit => Some(0),
            TyKind::Bool => Some(1),
            TyKind::Int(kind) => Some(kind.bytes().into()),
            TyKind::Float(kind) => Some(kind.bytes().into()),
            TyKind::Str => None,
            // TODO: Functions might be just usize
            TyKind::FuncDef(..) => todo!(),
            TyKind::Func(..) => todo!(),
            TyKind::Adt(adt) => adt.max_variant_size(),
            TyKind::Struct(data) => data.size(),
            TyKind::Ref(_) => Some(IntKind::Uint.bytes().into()),
            TyKind::Var(_) => panic!(),
            TyKind::Existential(_) => panic!(),
            TyKind::Forall(..) => panic!(),
            TyKind::Kind(_) => panic!(),
        }
    }
}

impl std::fmt::Debug for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ty({})", self)
    }
}

impl<'sess, 'a> std::fmt::Display for WithSess<'sess, 'a, Ty> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.value.kind() {
            TyKind::Error => write!(f, "?"),
            TyKind::Unit => write!(f, "()"),
            TyKind::Bool => write!(f, "bool"),
            TyKind::Int(kind) => write!(f, "{kind}"),
            TyKind::Float(kind) => write!(f, "{kind}"),
            TyKind::Str => write!(f, "str"),
            TyKind::FuncDef(def_id, params, body) => write!(
                f,
                "function {} of type {} -> {}",
                self.sess.def_table.def(*def_id).name(),
                params
                    .iter()
                    .map(|p| p.with_sess(self.sess).to_string())
                    .collect::<Vec<_>>()
                    .join(" - "),
                body.with_sess(self.sess)
            ),
            TyKind::Func(params, body) => write!(
                f,
                "{} -> {}",
                params
                    .iter()
                    .map(|p| p.with_sess(self.sess).to_string())
                    .collect::<Vec<_>>()
                    .join(" - "),
                body.with_sess(self.sess)
            ),
            TyKind::Adt(adt) => write!(f, "{}", self.sess.def_table.def(adt.def_id).name()),
            TyKind::Struct(struct_) => {
                write!(f, "{}", self.sess.def_table.def(struct_.def_id).name())
            },
            TyKind::Ref(inner) => write!(f, "ref {}", inner.with_sess(self.sess)),
            TyKind::Var(var) => write!(f, "{}", var.real_name()),
            // FIXME: Maybe not "?"
            TyKind::Existential(ex) => match ex.kind() {
                ExKind::Common => write!(f, "(?)"),
                ExKind::Int => write!(f, "int"),
                ExKind::Float => write!(f, "float"),
            },
            TyKind::Forall(alpha, body) => {
                write!(f, "∀{}. {}", alpha.real_name(), body.with_sess(self.sess))
            },
            TyKind::Kind(_) => todo!(),
        }
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl<'sess, 'a> WithColor for WithSess<'sess, 'a, Ty> {
    fn fg_color() -> Option<Color> {
        Ty::fg_color()
    }
}

impl WithColor for Ty {
    fn fg_color() -> Option<Color> {
        Some(Color::BrightBlue)
    }
}

// TODO: Move some tests to `ty_infer` mod
#[cfg(test)]
mod tests {
    // Ty tests //
    use super::{Ex, ExId, Field, Ty, TyVarId, Variant};
    use crate::{
        dt::idx::IndexVec,
        resolve::def::DefId,
        span::sym::{Ident, Internable},
        typeck::{
            builtin::{ty, TyMacroCtx},
            kind::{Kind, KindEx, KindExId},
            ty::{Adt, TyKind, DEFAULT_INT_KIND},
            ty_infer::{MonoTy, MonoTyKind},
        },
    };

    enum TyGen {
        Mono,
        Poly,
        Ex,
        Adt,
        Func,
        FuncDef,
        Var,
        KindTy,
    }

    fn gen(kind: TyGen) -> Ty {
        match kind {
            TyGen::Mono => super_simple_ty(),
            TyGen::Poly => Ty::forall(make_ty_var(), super_simple_ty()),
            TyGen::Ex => Ty::ex(make_ex()),
            TyGen::Adt => Ty::adt(make_adt()),
            TyGen::Func => Ty::func(None, vec![super_simple_ty()], super_simple_ty()),
            TyGen::FuncDef => Ty::func(
                Some(DefId::new(0)),
                vec![super_simple_ty()],
                super_simple_ty(),
            ),
            TyGen::Var => Ty::var(make_ty_var()),
            TyGen::KindTy => Ty::ty_kind(Kind::new_ty(super_simple_ty())),
        }
    }

    fn make_ex() -> Ex {
        Ex::common(ExId::new(0))
    }

    fn make_ty_var() -> TyVarId {
        TyVarId::new(0)
    }

    fn super_simple_ty() -> Ty {
        Ty::default_int()
    }

    fn make_field() -> Field {
        Field {
            name: Ident::synthetic("a".intern()),
            ty: super_simple_ty(),
        }
    }

    fn make_variant() -> Variant {
        Variant {
            def_id: DefId::new(0),
            name: Ident::synthetic("a".intern()),
            fields: IndexVec::from_iter([make_field()]),
        }
    }

    fn make_adt() -> Adt {
        Adt {
            def_id: DefId::new(0),
            variants: IndexVec::from_iter([make_variant()]),
        }
    }

    #[test]
    fn func_constructor_kind() {
        assert!(matches!(gen(TyGen::Func).kind(), TyKind::Func(..)));
        assert!(matches!(gen(TyGen::FuncDef).kind(), TyKind::FuncDef(..)));
    }

    #[test]
    fn as_ex() {
        let ex = make_ex();
        assert_eq!(Ty::ex(ex).as_ex(), Some(ex));

        assert!(matches!(super_simple_ty().as_ex(), None));
    }

    #[test]
    fn expect_kind() {
        Ty::ty_kind(Kind::new_ty(super_simple_ty())).expect_kind();
    }

    #[test]
    #[should_panic]
    fn expect_kind_fail() {
        super_simple_ty().expect_kind();
    }

    #[test]
    fn as_kind_ex() {
        let kind_ex = KindEx::new(KindExId::new(0));
        assert_eq!(
            Ty::ty_kind(Kind::new_ex(kind_ex)).as_kind_ex(),
            Some(kind_ex)
        );
    }

    #[test]
    fn is_ty() {
        assert!(super_simple_ty().is_ty());
        assert!(!Ty::ty_kind(Kind::new_ty(super_simple_ty())).is_ty());
    }

    #[test]
    fn is_kind() {
        assert!(Ty::ty_kind(Kind::new_ty(super_simple_ty())).is_kind());
        assert!(!super_simple_ty().is_kind());
    }

    #[test]
    fn is_mono() {
        assert!(super_simple_ty().is_mono());

        assert!(!gen(TyGen::Poly).is_mono());
        assert!(gen(TyGen::Var).is_mono());
    }

    #[test]
    fn is_func_like() {
        assert!(gen(TyGen::Func).is_func_like());
        assert!(gen(TyGen::FuncDef).is_func_like());
        assert!(!super_simple_ty().is_func_like());
    }

    #[test]
    fn is_adt() {
        assert!(Ty::adt(make_adt()).is_adt());
        assert!(!super_simple_ty().is_adt());
    }

    #[test]
    fn as_adt() {
        assert_eq!(Ty::adt(make_adt()).as_adt(), Ok(&make_adt()));
        assert!(matches!(super_simple_ty().as_adt(), Err(..)));
    }

    #[test]
    fn is_instantiated() {
        assert!(super_simple_ty().is_instantiated());
        assert!(!gen(TyGen::Ex).is_instantiated());
        assert!(!gen(TyGen::Poly).is_instantiated());
        assert!(!gen(TyGen::Var).is_instantiated());
        assert!(!gen(TyGen::KindTy).is_instantiated());
    }

    #[test]
    fn is_solved() {
        assert!(super_simple_ty().is_solved());
        assert!(!gen(TyGen::Ex).is_solved());
        assert!(gen(TyGen::Poly).is_solved());
        assert!(gen(TyGen::Var).is_solved());
        assert!(gen(TyGen::KindTy).is_solved());
    }

    #[test]
    fn as_mono() {
        assert_eq!(
            Ty::default_int().as_mono(),
            Some(MonoTy {
                sort: MonoTyKind::Int(DEFAULT_INT_KIND),
                ty: Ty::default_int()
            })
        );
    }

    #[test]
    fn contains_ex() {
        let ex = make_ex();
        assert!(Ty::forall(make_ty_var(), Ty::ex(ex)).contains_ex(ex));

        assert!(!Ty::default_int().contains_ex(ex));
    }

    #[test]
    fn mono() {
        gen(TyGen::Mono).mono();
    }

    #[test]
    #[should_panic]
    fn mono_failed() {
        gen(TyGen::Poly).mono();
    }

    #[test]
    fn mono_checked() {
        let mono = gen(TyGen::Mono);
        assert_eq!(mono.mono_checked(), mono);
    }

    #[test]
    #[should_panic]
    fn mono_checked_failed() {
        gen(TyGen::Poly).mono_checked();
    }

    #[test]
    fn func_def_id() {
        assert!(matches!(gen(TyGen::FuncDef).func_def_id(), Some(..)));
        assert!(matches!(gen(TyGen::Func).func_def_id(), None));
        assert!(matches!(super_simple_ty().func_def_id(), None));
    }

    #[test]
    fn maybe_add_func_def_id() {
        let def_id = DefId::new(0);
        let func = Ty::func(None, vec![super_simple_ty()], super_simple_ty());
        assert_eq!(
            func.maybe_add_func_def_id(def_id),
            Ty::func(Some(def_id), vec![super_simple_ty()], super_simple_ty())
        );
    }

    #[test]
    fn get_outer_ty_vars() {
        let mut ctx = TyMacroCtx::default();
        assert_eq!(
            ty!(@ctx ctx; forall a. a).outer_ty_vars(),
            vec![ctx.ty_var("a")]
        );
    }

    // use crate::session::SourceId;

    // const SOME_SOURCE_ID: SourceId = SourceId::new(123);

    // #[test]
    // fn same_ident_subst_eq() {
    //     let name = Ident::synthetic("lolkek".intern());
    //     assert_eq!(Subst::Var(name), name);
    // }

    // #[test]
    // fn diff_idents_subst_eq() {
    //     assert_eq!(
    //         Subst::Var(Ident::new(Span::new(0, 1, SOME_SOURCE_ID),
    // "a".intern())),         Ident::new(Span::new(123, 10,
    // SOME_SOURCE_ID), "a".intern())     )
    // }

    // #[test]
    // fn diff_idents_subst_ne() {
    //     assert_ne!(
    //         Subst::Var(Ident::new(Span::new(0, 1, SOME_SOURCE_ID),
    // "a".intern())),         Ident::new(Span::new(123, 10,
    // SOME_SOURCE_ID), "b".intern())     )
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
