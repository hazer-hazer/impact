use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
};

use crate::{
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, Idx},
    hir::{self, expr::Lit, HirId},
    resolve::def::Def,
    span::span::{Ident, Kw, Symbol},
};

use super::ctx::{Ctx, ExistentialId};

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

pub struct TyCtx {
    interner: TyInterner,
    ctx_stack: Vec<Ctx>,
    existential: ExistentialId,
    typed: HashMap<HirId, Ty>,
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            interner: Default::default(),
            ctx_stack: Default::default(),
            existential: ExistentialId::new(0),
            typed: HashMap::default(),
        }
    }

    pub fn ty(&self, ty: Ty) -> &TyS {
        self.interner.types.get(ty.as_usize()).unwrap()
    }

    // pub fn mono_poly_ty(&self, ty: Ty) -> MonoPolyTy {
    //     match self.ty(ty).kind() {
    //         TyKind::Error => MonoPolyTy::Mono(MonoTy::Error),
    //         TyKind::Unit => MonoPolyTy::Mono(MonoTy::Unit),
    //         &TyKind::Lit(lit) => MonoPolyTy::Mono(MonoTy::Lit(lit)),
    //         &TyKind::Var(var) => MonoPolyTy::Mono(MonoTy::Var(var)),
    //         &TyKind::Existential(ex) => MonoPolyTy::Mono(MonoTy::Existential(ex)),
    //         &TyKind::Func(param, body) => {
    //             let param = self.mono_poly_ty(param);
    //             let body = self.mono_poly_ty(body);
    //             if param.is_mono() && body.is_mono() {
    //                 MonoPolyTy::Mono(MonoTy::Func(
    //                     param.as_mono().unwrap(),
    //                     body.as_mono().unwrap(),
    //                 ))
    //             } else {
    //                 MonoPolyTy::Poly(PolyTy::Func(&param, &body))
    //             }
    //         },
    //         TyKind::Forall(_, _) => todo!(),
    //     }
    // }

    pub fn type_node(&mut self, id: HirId, ty: Ty) {
        assert!(self.typed.insert(id, ty).is_none());
    }

    // Type context //
    pub fn ctx(&mut self) -> &mut Ctx {
        self.ctx_stack.last_mut().unwrap()
    }

    pub fn enter_ctx(&mut self, ctx: Ctx) {
        self.ctx_stack.push(ctx);
    }

    pub fn exit_ctx(&mut self) {
        self.ctx_stack.pop();
    }

    pub fn under_new_ctx<T>(&mut self, f: impl FnMut(&mut Self) -> T) -> T {
        self.under_ctx(Ctx::default(), f)
    }

    pub fn under_ctx<T>(&mut self, ctx: Ctx, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.enter_ctx(ctx);
        let res = f(self);
        self.exit_ctx();

        res
    }

    /// Goes up from current context to the root looking for something in each scope
    fn ascending_ctx<T>(&self, mut f: impl FnMut(&Ctx) -> Option<T>) -> Option<T> {
        let mut ctx_index = self.ctx_stack.len() - 1;

        loop {
            let res = f(&self.ctx_stack[ctx_index]);
            if let Some(res) = res {
                return Some(res);
            }
            ctx_index -= 1;

            if ctx_index == 0 {
                break;
            }
        }

        None
    }

    pub fn fresh_ex(&mut self) -> ExistentialId {
        *self.existential.inc()
    }

    pub fn lookup_typed_term_ty(&self, name: Ident) -> Option<Ty> {
        self.ascending_ctx(|ctx: &Ctx| ctx.get_term(name))
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

    pub fn existential(&mut self, id: ExistentialId) -> Ty {
        self.intern(TyS::new(TyKind::Existential(id)))
    }

    // Type API //
    // FIXME: Maybe move to `Typeck`
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
    pub fn substitute(&mut self, ty: Ty, subst: Subst, with: Ty) -> Ty {
        match self.ty(ty).kind() {
            TyKind::Error | TyKind::Unit | TyKind::Lit(_) => ty,
            &TyKind::Var(ident) => {
                if subst == ident {
                    with
                } else {
                    ty
                }
            },
            &TyKind::Existential(ex) => {
                if subst == ex {
                    with
                } else {
                    ty
                }
            },
            &TyKind::Func(param_ty, return_ty) => {
                let param = self.substitute(param_ty, subst, with);
                let ret = self.substitute(return_ty, subst, with);
                self.func(param, ret)
            },
            &TyKind::Forall(ident, body) => {
                if subst == ident {
                    self.forall(ident, with)
                } else {
                    let subst = self.substitute(body, subst, with);
                    self.forall(ident, subst)
                }
            },
        }
    }

    /// Substitute all occurrences of universally quantified type inside it body
    pub fn open_forall(&mut self, ty: Ty, _subst: Ty) -> Ty {
        match self.ty(ty).kind() {
            &TyKind::Forall(alpha, body) => self.substitute(ty, Subst::Name(alpha), body),
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

    pub fn apply_on(&mut self, ty: Ty) -> Ty {
        match self.ty(ty).kind() {
            TyKind::Error | TyKind::Unit | TyKind::Lit(_) | TyKind::Var(_) => ty,
            TyKind::Existential(id) => self
                .ascending_ctx(|ctx| ctx.get_solution(*id))
                .unwrap_or(ty),
            &TyKind::Func(param_ty, return_ty) => {
                let param = self.apply_on(param_ty);
                let ret = self.apply_on(return_ty);
                self.func(param, ret)
            },
            &TyKind::Forall(ident, body) => {
                let body = self.apply_on(body);
                self.forall(ident, body)
            },
        }
    }

    pub fn occurs_in(&self, ty: Ty, name: Subst) -> bool {
        match self.ty(ty).kind() {
            TyKind::Error | TyKind::Unit | TyKind::Lit(_) => false,
            &TyKind::Var(name_) if name == name_ => true,
            TyKind::Var(_) => false,
            &TyKind::Existential(id) if name == id => {
                // TODO: Is this right?!
                true
            },
            TyKind::Existential(_) => false,
            &TyKind::Func(param, ret) => self.occurs_in(param, name) || self.occurs_in(ret, name),
            &TyKind::Forall(alpha, _) if name == alpha => true,
            &TyKind::Forall(_, body) => self.occurs_in(body, name),
        }
    }

    pub fn ty_wf(&mut self, ty: Ty) -> TyResult<()> {
        match self.ty(ty).kind() {
            &TyKind::Var(ident) => {
                if self.ascending_ctx(|ctx| ctx.get_var(ident)).is_some() {
                    Ok(())
                } else {
                    Err(TyError())
                }
            },
            &TyKind::Existential(id) => {
                if self.ascending_ctx(|ctx| ctx.get_ex(id)).is_some() {
                    Ok(())
                } else {
                    Err(TyError())
                }
            },
            &TyKind::Func(param_ty, return_ty) => {
                self.ty_wf(param_ty)?;
                self.ty_wf(return_ty)
            },
            &TyKind::Forall(ident, body) => self.under_new_ctx(|this| {
                let alpha = this.var(ident);
                let open_forall = this.open_forall(body, alpha);
                this.ty_wf(open_forall)?;
                Ok(())
            }),
            _ => Err(TyError()),
        }
    }
}
