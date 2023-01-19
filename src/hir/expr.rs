use std::fmt::Display;

use crate::{
    ast::{
        expr::{InfixOp, PrefixOp},
        NodeId,
    },
    span::span::{Span, Symbol, WithSpan},
};

use super::{pat::Pat, stmt::Stmt, ty::Ty, HirId, Path, N};

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

pub struct Expr {
    id: HirId,
    kind: ExprKind,
    span: Span,
}

impl Expr {
    pub fn new(id: HirId, kind: ExprKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }
}

impl WithSpan for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

pub struct Block {
    node_id: NodeId,
    stmts: Vec<Stmt>,
    expr: Option<N<Expr>>,
}

impl Block {
    pub fn new(node_id: NodeId, stmts: Vec<Stmt>, expr: Option<N<Expr>>) -> Self {
        Self {
            node_id,
            stmts,
            expr,
        }
    }

    pub fn stmts(&self) -> &[Stmt] {
        self.stmts.as_ref()
    }

    pub fn expr(&self) -> Option<&N<Expr>> {
        self.expr.as_ref()
    }
}

pub struct PathExpr(pub Path);

pub struct Lambda {
    pub param: Pat,
    pub body: N<Expr>,
}

pub struct TyExpr {
    pub expr: N<Expr>,
    pub ty: N<Ty>,
}

pub struct Call {
    pub lhs: N<Expr>,
    pub arg: N<Expr>,
}

pub struct Infix {
    pub lhs: N<Expr>,
    pub op: InfixOp,
    pub rhs: N<Expr>,
}

pub struct Prefix {
    pub op: PrefixOp,
    pub rhs: N<Expr>,
}

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

pub struct Param {
    pat: Pat,
}

pub struct Body {
    params: Vec<Param>,
    value: Expr,
}
