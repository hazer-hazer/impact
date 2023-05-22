use std::fmt::Display;

use super::{impl_with_hir_id, Block, BodyId, Expr, ExprPath, HirId, Stmt, Ty, WithHirId};
use crate::{
    resolve::{builtin::ValueBuiltin, def::DefId},
    span::{impl_with_span, sym::Symbol, Span, WithSpan},
};

/// Get span of expression result. Used in typeck.
/// Example: Last expression-statement in block
pub trait ResultSpan {
    fn result_span(&self) -> Span;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntKind {
    Unknown,

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

impl Display for IntKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntKind::Unknown => "",
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FloatKind {
    Unknown,

    F32,
    F64,
}

impl Display for FloatKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FloatKind::Unknown => "",
                FloatKind::F32 => "f32",
                FloatKind::F64 => "f64",
            }
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Lit {
    Bool(bool),
    Int(u64, IntKind),
    Float(f64, FloatKind),
    String(Symbol),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Bool(val) => write!(f, "{}", if *val { "true" } else { "false" }),
            Lit::Int(val, kind) => write!(f, "{}{}", val, kind),
            Lit::Float(val, kind) => write!(f, "{}{}", val, kind),
            Lit::String(val) => write!(f, "\"{}\"", val),
        }
    }
}

#[derive(Debug)]
pub struct BlockNode {
    id: HirId,
    stmts: Vec<Stmt>,
    expr: Option<Expr>,
    span: Span,
}

impl_with_span!(BlockNode);
impl_with_hir_id!(BlockNode);

impl BlockNode {
    pub fn new(id: HirId, stmts: Vec<Stmt>, expr: Option<Expr>, span: Span) -> Self {
        Self {
            id,
            stmts,
            expr,
            span,
        }
    }

    pub fn stmts(&self) -> &[Stmt] {
        self.stmts.as_ref()
    }

    pub fn expr(&self) -> Option<Expr> {
        self.expr
    }
}

#[derive(Debug)]
pub struct Arm {}

#[derive(Debug)]
pub struct Lambda {
    pub def_id: DefId,
    pub body_id: BodyId,
}

#[derive(Debug)]
pub struct TyExpr {
    pub expr: Expr,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Call {
    pub lhs: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(Lit),
    Path(ExprPath),
    Block(Block),
    Lambda(Lambda),
    Call(Call),
    Let(Block),
    Ty(TyExpr),
    // FieldAccess(Expr, Ident),
    Builtin(ValueBuiltin),
}

#[derive(Debug)]
pub struct ExprNode {
    id: HirId,
    pub kind: ExprKind,
    span: Span,
}

impl_with_span!(ExprNode);

impl ExprNode {
    pub fn new(id: HirId, kind: ExprKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn id(&self) -> HirId {
        self.id
    }
}

impl_with_hir_id!(ExprNode);
