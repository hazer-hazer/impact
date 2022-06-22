use std::fmt::Display;

use crate::{
    parser::token::{Infix, Prefix, Token, TokenKind},
    span::span::{Ident, Span, Spanned, Symbol, WithSpan},
};

use super::{stmt::Stmt, ty::Ty, NodeId, N, PR};

pub struct Expr {
    id: NodeId,
    kind: ExprKind,
    span: Span,
}

impl Expr {
    pub fn new(id: NodeId, kind: ExprKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn id(&self) -> NodeId {
        self.id
    }
}

impl WithSpan for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy)]
pub enum Lit {
    Bool(bool),
    Int(i64),
    String(Symbol),
}

pub type InfixOp = Spanned<InfixOpKind>;

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

impl Display for InfixOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InfixOpKind::Plus => "+",
                InfixOpKind::Minus => "-",
                InfixOpKind::Mul => "*",
                InfixOpKind::Div => "/",
                InfixOpKind::Mod => "%",
            }
        )
    }
}

pub type PrefixOp = Spanned<PrefixOpKind>;

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

impl Display for PrefixOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PrefixOpKind::Not => "not",
            }
        )
    }
}

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
    Ty(PR<N<Expr>>, PR<N<Ty>>),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Bool(val) => write!(f, "{}", if *val { "true" } else { "false" }),
            Lit::Int(val) => write!(f, "{}", val),
            Lit::String(val) => write!(f, "{}", val),
        }
    }
}

// impl Display for ExprKind {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             match self {
//                 ExprKind::Lit(_) => todo!(),
//                 ExprKind::Ident(_) => todo!(),
//                 ExprKind::Infix(_, _, _) => todo!(),
//                 ExprKind::Prefix(_, _) => todo!(),
//                 ExprKind::App(_, _) => todo!(),
//                 ExprKind::Block(_) => todo!(),
//                 ExprKind::Let(_, _, _) => todo!(),
//                 ExprKind::Abs(_, _) => todo!(),
//                 ExprKind::Ty(_, _) => todo!(),
//                 _ => ""
//             }
//         )
//     }
// }
