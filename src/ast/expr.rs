use std::fmt::Display;

use crate::{
    parser::token::{Infix, Prefix, Token, TokenKind},
    span::span::{Ident, Span, Spanned, Symbol, WithSpan},
};

use super::{pr_display, stmt::Stmt, ty::Ty, NodeId, NodeKindStr, Path, WithNodeId, N, PR};

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
}

impl WithNodeId for Expr {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl WithSpan for Expr {
    fn span(&self) -> Span {
        self.span
    }
}

impl NodeKindStr for Expr {
    fn kind_str(&self) -> String {
        self.kind().kind_str()
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

pub struct Block {
    id: NodeId,
    stmts: Vec<PR<N<Stmt>>>,
    expr: PR<N<Expr>>,
    span: Span,
}

impl Block {
    pub fn new(id: NodeId, stmts: Vec<PR<N<Stmt>>>, expr: PR<N<Expr>>, span: Span) -> Self {
        Self {
            id,
            stmts,
            expr,
            span,
        }
    }

    pub fn stmts(&self) -> &[PR<N<Stmt>>] {
        self.stmts.as_ref()
    }

    pub fn expr(&self) -> &PR<N<Expr>> {
        &self.expr
    }
}

impl WithNodeId for Block {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl WithSpan for Block {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            self.stmts
                .iter()
                .map(|stmt| format!("{}", pr_display(stmt)))
                .collect::<Vec<_>>()
                .join("\n"),
            pr_display(&self.expr)
        )
    }
}

pub enum ExprKind {
    Lit(Lit),
    Path(PR<Path>),
    Block(PR<Block>),
    Infix(PR<N<Expr>>, InfixOp, PR<N<Expr>>),
    Prefix(PrefixOp, PR<N<Expr>>),
    Abs(PR<Ident>, PR<N<Expr>>),
    App(PR<N<Expr>>, PR<N<Expr>>),
    Let(PR<Block>),
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

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Lit(lit) => write!(f, "{}", lit),
            ExprKind::Path(path) => write!(f, "{}", pr_display(path)),
            ExprKind::Block(block) => write!(f, "{}", pr_display(block)),
            ExprKind::Infix(lhs, op, rhs) => {
                write!(f, "{} {} {}", pr_display(lhs), op, pr_display(rhs))
            }
            ExprKind::Prefix(op, rhs) => write!(f, "{}{}", op, pr_display(rhs)),
            ExprKind::Abs(param_name, body) => {
                write!(f, "{} -> {}", pr_display(param_name), pr_display(body))
            }
            ExprKind::App(lhs, arg) => write!(f, "{} {}", pr_display(lhs), pr_display(arg)),
            ExprKind::Let(block) => write!(f, "{}", pr_display(block)),
            ExprKind::Ty(expr, ty) => write!(f, "{}: {}", pr_display(expr), pr_display(ty)),
        }
    }
}

impl NodeKindStr for ExprKind {
    fn kind_str(&self) -> String {
        match self {
            ExprKind::Lit(_) => "literal".to_string(),
            ExprKind::Block(_) => "block expression".to_string(),
            ExprKind::Path(_) => "path".to_string(),
            ExprKind::Abs(_, _) => "lambda".to_string(),
            ExprKind::App(_, _) => "function call".to_string(),
            ExprKind::Let(_) => "let expression".to_string(),
            ExprKind::Ty(_, _) => "type ascription".to_string(),
            ExprKind::Infix(_, _, _) => "infix expression".to_string(),
            ExprKind::Prefix(_, _) => "prefix expression".to_string(),
        }
    }
}
