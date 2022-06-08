use crate::{
    pp::PP,
    span::span::{Ident, Span, Spanned, Symbol},
};

use super::N;

pub struct Expr {
    span: Span,
    kind: ExprKind,
}

#[derive(Clone, Copy)]
pub enum Lit {
    Bool(bool),
    Int(i64),
    String(Symbol),
}

pub enum InfixOpKind {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

pub type InfixOp = Spanned<InfixOpKind>;

pub enum PrefixOpKind {
    Not,
}

pub type PrefixOp = Spanned<PrefixOpKind>;

pub struct App {
    lhs: Expr,
    args: Vec<Expr>,
}

pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Infix(N<Expr>, InfixOp, N<Expr>),
    Prefix(PrefixOp, N<Expr>),
}

impl<'a> PP<'a> for Expr {
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        match &self.kind {
            ExprKind::Lit(lit) => lit.ppfmt(sess),
            ExprKind::Ident(sym) => sym.ppfmt(sess),
            ExprKind::Infix(lhs, op, rhs) => {
                format!("{} {} {}", lhs.ppfmt(sess), op.ppfmt(sess), rhs.ppfmt(sess))
            }
            ExprKind::Prefix(op, rhs) => format!("{} {}", op.ppfmt(sess), rhs.ppfmt(sess)),
        }
    }
}

impl<'a> PP<'a> for InfixOpKind {
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        match self {
            InfixOpKind::Plus => "+",
            InfixOpKind::Minus => "-",
            InfixOpKind::Mul => "*",
            InfixOpKind::Div => "/",
            InfixOpKind::Mod => "%",
        }
        .to_string()
    }
}

impl<'a> PP<'a> for PrefixOpKind {
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        match self {
            PrefixOpKind::Not => "not",
        }
        .to_string()
    }
}

impl<'a> PP<'a> for Lit {
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        match self {
            Lit::Bool(val) => if *val { "true" } else { "false" }.to_string(),
            Lit::Int(val) => val.to_string(),
            Lit::String(val) => val.ppfmt(sess),
        }
    }
}
