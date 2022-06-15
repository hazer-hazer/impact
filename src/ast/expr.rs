use crate::{
    parser::token::{Infix, Prefix, Token, TokenKind},
    pp::PP,
    session::Session,
    span::span::{Ident, Spanned, Symbol},
};

use super::{stmt::Stmt, N, PR, ty::Ty};

pub type Expr = Spanned<ExprKind>;

#[derive(Clone, Copy)]
pub enum Lit {
    Bool(bool),
    Int(i64),
    String(Symbol),
}

#[derive(Clone, Copy)]
pub enum InfixOpKind {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

impl InfixOpKind {
    pub fn from_tok(tok: Token) -> Spanned<Self> {
        Spanned::new(
            tok.span,
            match tok.kind {
                TokenKind::Infix(infix) => match infix {
                    Infix::Plus => InfixOpKind::Plus,
                    Infix::Minus => InfixOpKind::Minus,
                    Infix::Mul => InfixOpKind::Mul,
                    Infix::Div => InfixOpKind::Div,
                    Infix::Mod => InfixOpKind::Mod,
                },
                _ => panic!("Cannot make InfixOpKind from not a Infix Token"),
            },
        )
    }
}

pub type InfixOp = Spanned<InfixOpKind>;

#[derive(Clone, Copy)]
pub enum PrefixOpKind {
    Not,
}

impl PrefixOpKind {
    pub fn from_tok(tok: &Token) -> Spanned<Self> {
        Spanned::new(
            tok.span,
            match tok.kind {
                TokenKind::Prefix(prefix) => match prefix {
                    Prefix::Not => PrefixOpKind::Not,
                },
                _ => panic!("Cannot make PrefixOpKind from not a Prefix Token"),
            },
        )
    }
}

pub type PrefixOp = Spanned<PrefixOpKind>;

pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Infix(PR<N<Expr>>, InfixOp, PR<N<Expr>>),
    Prefix(PrefixOp, PR<N<Expr>>),
    Abs(PR<Ident>, PR<N<Expr>>),
    // App(PR<N<Expr>>, Vec<PR<N<Expr>>>),
    App(PR<N<Expr>>, PR<N<Expr>>),
    Block(Vec<PR<N<Stmt>>>),
    Let(PR<Ident>, PR<N<Expr>>, PR<N<Expr>>),
    Type(PR<N<Expr>>, PR<N<Ty>>),
}

impl<'a> PP<'a> for InfixOpKind {
    fn ppfmt(&self, _: &'a Session) -> String {
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
    fn ppfmt(&self, _: &'a Session) -> String {
        match self {
            PrefixOpKind::Not => "not",
        }
        .to_string()
    }
}

impl<'a> PP<'a> for Lit {
    fn ppfmt(&self, sess: &'a Session) -> String {
        match self {
            Lit::Bool(val) => if *val { "true" } else { "false" }.to_string(),
            Lit::Int(val) => val.to_string(),
            Lit::String(val) => val.ppfmt(sess),
        }
    }
}

impl<'a> PP<'a> for ExprKind {
    fn ppfmt(&self, _: &'a Session) -> String {
        match self {
            ExprKind::Lit(_) => todo!(),
            ExprKind::Ident(_) => todo!(),
            ExprKind::Infix(_, _, _) => todo!(),
            ExprKind::Prefix(_, _) => todo!(),
            ExprKind::App(_, _) => todo!(),
            ExprKind::Block(_) => todo!(),
            ExprKind::Let(_, _, _) => todo!(),
            ExprKind::Abs(_, _) => todo!(),
            ExprKind::Type(_, _) => todo!(),
        }
    }
}
