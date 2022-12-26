use std::fmt::Display;

use crate::{
    ast::expr::{InfixOp, PrefixOp},
    span::span::{Span, Symbol, WithSpan},
};

use super::{pat::Pat, stmt::Stmt, ty::Ty, Path, N};

pub use crate::typeck::ty::{FloatKind, IntKind};

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
    kind: ExprKind,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
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
    stmts: Vec<Stmt>,
    expr: Option<N<Expr>>,
}

impl Block {
    pub fn new(stmts: Vec<Stmt>, expr: Option<N<Expr>>) -> Self {
        Self { stmts, expr }
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
    pub ty: Ty,
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
