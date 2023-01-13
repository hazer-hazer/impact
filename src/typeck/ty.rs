use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
};

use crate::{
    ast::expr::Lit,
    cli::color::{Color, Colorize},
    dt::idx::declare_idx,
    parser::token,
    span::span::{Ident, Kw, Symbol},
};

use super::ctx::{Ctx, CtxItem, CtxItemName, ExistentialId, ExistentialIdInner};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeVarId(pub usize);

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

impl From<token::IntKind> for IntKind {
    fn from(kind: token::IntKind) -> Self {
        match kind {
            token::IntKind::Unknown => todo!(),
            token::IntKind::U8 => Self::U8,
            token::IntKind::U16 => Self::U16,
            token::IntKind::U32 => Self::U32,
            token::IntKind::U64 => Self::U64,
            token::IntKind::Uint => Self::Uint,
            token::IntKind::I8 => Self::I8,
            token::IntKind::I16 => Self::I16,
            token::IntKind::I32 => Self::I32,
            token::IntKind::I64 => Self::I64,
            token::IntKind::Int => Self::Int,
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

impl From<token::FloatKind> for FloatKind {
    fn from(kind: token::FloatKind) -> Self {
        match kind {
            token::FloatKind::Unknown => todo!(),
            token::FloatKind::F32 => Self::F32,
            token::FloatKind::F64 => Self::F64,
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

impl From<&Lit> for PrimTy {
    fn from(lit: &Lit) -> Self {
        match lit {
            Lit::Bool(_) => Self::Bool,
            Lit::Int(_, kind) => Self::Int(IntKind::from(*kind)),
            Lit::Float(_, kind) => Self::Float(FloatKind::from(*kind)),
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

    fn intern(&mut self, ty: TyS) -> TyId {
        let hash = Self::hash(&ty);

        if let Some(id) = self.map.get(&hash) {
            return *id;
        }

        let id = TyId::from(self.types.len());

        self.map.insert(hash, id);
        self.types.push(ty);

        id
    }
}

pub struct TyCtx {
    interner: TyInterner,
    ctx: Ctx,
    existential: ExistentialIdInner,
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            interner: Default::default(),
            ctx: Ctx::initial(),
            existential: 0,
        }
    }

    pub fn ty(&self, ty: Ty) -> &TyS {
        self.interner.types.get(ty.as_usize()).unwrap()
    }

    // Type context //
    pub fn fresh_ex(&mut self) -> ExistentialId {
        let ex = ExistentialId::new(self.existential);
        self.existential += 1;
        ex
    }

    pub fn lookup_typed_term_ty(&self, name: Ident) -> Option<Ty> {
        if let Some(item) = self.ctx.lookup(CtxItemName::TypedTerm(name)) {
            match item {
                &CtxItem::TypedTerm(_, ty) => Some(ty),
                _ => unreachable!(),
            }
        } else {
            None
        }
    }

    // Constructors //
    pub fn intern(&mut self, ty: TyS) -> Ty {
        self.interner.intern(ty)
    }

    pub fn unit(&mut self) -> Ty {
        self.interner.intern(TyS::new(TyKind::Unit))
    }

    pub fn error(&mut self) -> Ty {
        self.interner.intern(TyS::new(TyKind::Error))
    }

    pub fn var(&mut self, ident: Ident) -> Ty {
        self.interner.intern(TyS::new(TyKind::Var(ident)))
    }

    pub fn lit(&mut self, prim: PrimTy) -> Ty {
        self.interner.intern(TyS::new(TyKind::Lit(prim)))
    }

    pub fn func(&mut self, param: Ty, ret: Ty) -> Ty {
        self.intern(TyS::new(TyKind::Func(param, ret)))
    }

    pub fn forall(&mut self, alpha: Ident, body: Ty) -> Ty {
        self.intern(TyS::new(TyKind::Forall(alpha, body)))
    }

    // Type API //
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

    /// Substitute
    pub fn substitute(&mut self, ty: Ty, name: Ident, with: Ty) -> Ty {
        match self.ty(ty).kind() {
            TyKind::Error | TyKind::Unit | TyKind::Lit(_) => ty,
            &TyKind::Var(ident) => {
                if name == ident {
                    with
                } else {
                    ty
                }
            },
            TyKind::Existential(_) => ty,
            &TyKind::Func(param_ty, return_ty) => {
                let param = self.substitute(param_ty, name, with);
                let ret = self.substitute(return_ty, name, with);
                self.func(param, ret)
            },
            &TyKind::Forall(ident, body) => {
                if name == ident {
                    self.forall(ident, with)
                } else {
                    let subst = self.substitute(body, name, with);
                    self.forall(ident, subst)
                }
            },
        }
    }

    /// Substitute all occurrences of universally quantified type inside it body
    pub fn open_forall(&mut self, ty: Ty, _subst: Ty) -> Ty {
        match self.ty(ty).kind() {
            &TyKind::Forall(ident, body) => self.substitute(ty, ident, body),
            _ => unreachable!(),
        }
    }

    // pub fn substitute_existentials(&self, existentials: &HashMap<Ident, Ty>) -> Ty {
    //     match self.kind() {
    //         TyKind::Error | TyKind::Unit | TyKind::Lit(_) | TyKind::Var(_) => self.clone(),
    //         TyKind::Existential(id) => match existentials.get(ident) {
    //             Some(ty) => ty.clone(),
    //             None => self.clone(),
    //         },
    //         TyKind::Func(param_ty, return_ty) => Ty::new(TyKind::Func(
    //             Box::new(param_ty.substitute_existentials(existentials)),
    //             Box::new(return_ty.substitute_existentials(existentials)),
    //         )),
    //         TyKind::Forall(ident, ty) => Ty::new(TyKind::Forall(
    //             *ident,
    //             Box::new(ty.substitute_existentials(existentials)),
    //         )),
    //     }
    // }

    pub fn apply_ctx(&mut self, ty: Ty) -> Ty {
        match self.ty(ty).kind() {
            TyKind::Error | TyKind::Unit | TyKind::Lit(_) | TyKind::Var(_) => ty,
            TyKind::Existential(id) => match self.ctx.lookup(CtxItemName::Existential(*id)) {
                Some(item) => match item {
                    CtxItem::TypedTerm(_, ty) => ty.clone(),
                    CtxItem::Existential(_, ety) => {
                        if let Some(ty) = ety {
                            ty.clone()
                        } else {
                            ty
                        }
                    },
                    _ => ty,
                },
                None => ty,
            },
            &TyKind::Func(param_ty, return_ty) => {
                let param = self.apply_ctx(param_ty);
                let ret = self.apply_ctx(return_ty);
                self.func(param, ret)
            },
            &TyKind::Forall(ident, body) => {
                let body = self.apply_ctx(body);
                self.forall(ident, body)
            },
        }
    }

    pub fn occurs_in(&self, ty: Ty, name: Ident) -> bool {
        match self.ty(ty).kind() {
            TyKind::Error | TyKind::Unit | TyKind::Lit(_) => false,
            TyKind::Var(name_) if name_.sym() == name.sym() => true,
            TyKind::Var(_) => false,
            TyKind::Existential(_) => {
                // TODO: Is this right?!
                false
            },
            &TyKind::Func(param, ret) => self.occurs_in(param, name) || self.occurs_in(ret, name),
            &TyKind::Forall(alpha, _) if alpha.sym() == name.sym() => true,
            &TyKind::Forall(_, body) => self.occurs_in(body, name),
        }
    }

    pub fn ty_wf(&mut self, ty: Ty) -> TyResult<()> {
        match self.ty(ty).kind() {
            &TyKind::Var(ident) => {
                if self.ctx.contains(CtxItemName::Var(ident)) {
                    Ok(())
                } else {
                    Err(TyError())
                }
            },
            &TyKind::Existential(ident) => {
                if self.ctx.contains(CtxItemName::Existential(ident)) {
                    Ok(())
                } else {
                    Err(TyError())
                }
            },
            &TyKind::Func(param_ty, return_ty) => {
                self.ty_wf(param_ty)?;
                self.ty_wf(return_ty)
            },
            &TyKind::Forall(ident, body) => {
                let marker_name = Ident::synthetic(Symbol::from_kw(Kw::M));
                self.ctx.enter(marker_name, vec![CtxItem::Var(ident)]);
                let alpha = self.var(ident);
                let open_forall = self.open_forall(body, alpha);
                self.ty_wf(open_forall)?;
                self.ctx.leave(marker_name);
                Ok(())
            },
            _ => Err(TyError()),
        }
    }

    pub fn ctx(&mut self) -> &mut Ctx {
        &mut self.ctx
    }
}
