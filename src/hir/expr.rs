use std::fmt::Display;

use crate::{
    ast::expr::{InfixOp, PrefixOp},
    span::span::{Span, Symbol, WithSpan},
};

use super::{pat::Pat, stmt::Stmt, ty::Ty, Path};

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

pub struct Expr<'hir> {
    kind: ExprKind<'hir>,
    span: Span,
}

impl<'hir> Expr<'hir> {
    pub fn new(kind: ExprKind<'hir>, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }
}

impl<'hir> WithSpan for Expr<'hir> {
    fn span(&self) -> Span {
        self.span
    }
}

pub struct Block<'hir> {
    stmts: &'hir [&'hir Stmt<'hir>],
    expr: Option<&'hir Expr<'hir>>,
}

impl<'hir> Block<'hir> {
    pub fn new(stmts: &'hir [&'hir Stmt<'hir>], expr: Option<&'hir Expr>) -> Self {
        Self { stmts, expr }
    }

    pub fn stmts(&self) -> &[&Stmt] {
        self.stmts.as_ref()
    }

    pub fn expr(&self) -> Option<&Expr> {
        self.expr
    }
}

pub struct PathExpr<'hir>(pub &'hir Path<'hir>);

pub struct Lambda<'hir> {
    pub param: &'hir Pat<'hir>,
    pub body: &'hir Expr<'hir>,
}

pub struct TyExpr<'hir> {
    pub expr: &'hir Expr<'hir>,
    pub ty: &'hir Ty<'hir>,
}

pub struct Call<'hir> {
    pub lhs: &'hir Expr<'hir>,
    pub arg: &'hir Expr<'hir>,
}

pub struct Infix<'hir> {
    pub lhs: &'hir Expr<'hir>,
    pub op: InfixOp,
    pub rhs: &'hir Expr<'hir>,
}

pub struct Prefix<'hir> {
    pub op: PrefixOp,
    pub rhs: &'hir Expr<'hir>,
}

pub enum ExprKind<'hir> {
    Unit,
    Lit(Lit),
    Path(PathExpr<'hir>),
    Block(&'hir Block<'hir>),
    Infix(Infix<'hir>),
    Prefix(Prefix<'hir>),
    Lambda(Lambda<'hir>),
    Call(Call<'hir>),
    Let(&'hir Block<'hir>),
    Ty(TyExpr<'hir>),
}

pub struct Param<'hir> {
    pat: &'hir Pat<'hir>,
}

pub struct Body<'hir> {
    params: &'hir [Param<'hir>],
    value: &'hir Expr<'hir>,
}
