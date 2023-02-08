use std::fmt::Display;

use crate::{
    ast::expr::{InfixOp, PrefixOp},
    span::span::{Span, Symbol, WithSpan},
};

use super::{
    pat::{Pat, PatNode},
    stmt::Stmt,
    ty::Ty,
    HirId, Path, WithHirId,
};

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
            Lit::String(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug)]
pub struct ExprNode {
    id: HirId,
    kind: ExprKind,
    span: Span,
}

impl ExprNode {
    pub fn new(id: HirId, kind: ExprKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }
}

impl WithHirId for ExprNode {
    fn id(&self) -> HirId {
        self.id
    }
}

impl WithSpan for ExprNode {
    fn span(&self) -> Span {
        self.span
    }
}

pub type Expr = HirId;

#[derive(Debug)]
pub struct BlockNode {
    id: HirId,
    stmts: Vec<Stmt>,
    expr: Option<Expr>,
}

impl BlockNode {
    pub fn new(id: HirId, stmts: Vec<Stmt>, expr: Option<Expr>) -> Self {
        Self { id, stmts, expr }
    }

    pub fn stmts(&self) -> &[Stmt] {
        self.stmts.as_ref()
    }

    pub fn expr(&self) -> Option<&Expr> {
        self.expr.as_ref()
    }
}

impl WithHirId for BlockNode {
    fn id(&self) -> HirId {
        self.id
    }
}

pub type Block = HirId;

#[derive(Debug)]
pub struct PathExpr(pub Path);

#[derive(Debug)]
pub struct Lambda {
    pub param: Pat,
    pub body: Expr,
}

#[derive(Debug)]
pub struct TyExpr {
    pub expr: Expr,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Call {
    pub lhs: Expr,
    pub arg: Expr,
}

#[derive(Debug)]
pub struct Infix {
    pub lhs: Expr,
    pub op: InfixOp,
    pub rhs: Expr,
}

#[derive(Debug)]
pub struct Prefix {
    pub op: PrefixOp,
    pub rhs: Expr,
}

#[derive(Debug)]
pub enum ExprKind {
    Unit,
    Lit(Lit),
    Path(PathExpr),
    Block(Block),
    Infix(Infix),
    Prefix(Prefix),
    Lambda(Lambda),
    Call(Call),
    Let(Block),
    Ty(TyExpr),
}

#[derive(Debug)]
pub struct Param {
    pat: PatNode,
}

#[derive(Debug)]
pub struct Body {
    params: Vec<Param>,
    value: ExprNode,
}
