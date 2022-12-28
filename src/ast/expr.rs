use std::fmt::Display;

use crate::{
    parser::token::{self, FloatKind, IntKind, Token, TokenKind},
    span::span::{Span, Spanned, Symbol, WithSpan},
};

use super::{
    pat::Pat, pr_display, stmt::Stmt, ty::Ty, NodeId, NodeKindStr, Path, WithNodeId, N, PR,
};

macro_rules! is_block_ended {
    ($pr: expr) => {
        match &$pr {
            Err(_) => false,
            Ok(node) => node.is_block_ended(),
        }
    };
}

pub(crate) use is_block_ended;

#[derive(Debug)]
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

    pub fn is_block_ended(&self) -> bool {
        self.kind.is_block_ended()
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Lit {
    Bool(bool),
    Int(u64, IntKind),
    Float(f64, FloatKind),
    String(Symbol),
}

pub type InfixOp = Spanned<InfixOpKind>;

#[derive(Clone, Copy, Debug)]
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
                    token::Infix::Plus => InfixOpKind::Plus,
                    token::Infix::Minus => InfixOpKind::Minus,
                    token::Infix::Mul => InfixOpKind::Mul,
                    token::Infix::Div => InfixOpKind::Div,
                    token::Infix::Mod => InfixOpKind::Mod,
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

#[derive(Clone, Copy, Debug)]
pub enum PrefixOpKind {
    Not,
}

impl PrefixOpKind {
    pub fn from_tok(tok: &Token) -> Spanned<Self> {
        Spanned::new(
            tok.span,
            match tok.kind {
                TokenKind::Prefix(prefix) => match prefix {
                    token::Prefix::Not => PrefixOpKind::Not,
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

#[derive(Debug)]
pub struct Block {
    id: NodeId,
    stmts: Vec<PR<N<Stmt>>>,
    span: Span,
}

impl Block {
    pub fn new(id: NodeId, stmts: Vec<PR<N<Stmt>>>, span: Span) -> Self {
        Self { id, stmts, span }
    }

    pub fn stmts(&self) -> &[PR<N<Stmt>>] {
        self.stmts.as_ref()
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
            "{}",
            self.stmts
                .iter()
                .map(|stmt| format!("{}", pr_display(stmt)))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Debug)]
pub struct PathExpr(pub PR<Path>);

#[derive(Debug)]
pub struct Infix {
    pub lhs: PR<N<Expr>>,
    pub op: InfixOp,
    pub rhs: PR<N<Expr>>,
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            pr_display(&self.lhs),
            self.op,
            pr_display(&self.rhs)
        )
    }
}

#[derive(Debug)]
pub struct Prefix {
    pub op: PrefixOp,
    pub rhs: PR<N<Expr>>,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, pr_display(&self.rhs))
    }
}

#[derive(Debug)]
pub struct Lambda {
    pub param: PR<Pat>,
    pub body: PR<N<Expr>>,
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            pr_display(&self.param),
            pr_display(&self.body)
        )
    }
}

#[derive(Debug)]
pub struct Call {
    pub lhs: PR<N<Expr>>,
    pub arg: PR<N<Expr>>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", pr_display(&self.lhs), pr_display(&self.arg))
    }
}

#[derive(Debug)]
pub struct TyExpr {
    pub expr: PR<N<Expr>>,
    pub ty: PR<N<Ty>>,
}

impl Display for TyExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", pr_display(&self.expr), pr_display(&self.ty))
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Unit,
    Lit(Lit),
    Path(PathExpr),
    Block(PR<Block>),
    Infix(Infix),
    Prefix(Prefix),
    Lambda(Lambda),
    Call(Call),
    Let(PR<Block>),
    Ty(TyExpr),
}

impl ExprKind {
    pub fn is_block_ended(&self) -> bool {
        match self {
            ExprKind::Unit | ExprKind::Lit(_) | ExprKind::Path(_) | ExprKind::Ty(_) => false,
            ExprKind::Block(_) | ExprKind::Let(_) => true,
            ExprKind::Infix(infix) => is_block_ended!(infix.rhs),
            ExprKind::Prefix(prefix) => is_block_ended!(prefix.rhs),
            ExprKind::Lambda(lambda) => is_block_ended!(lambda.body),
            ExprKind::Call(call) => is_block_ended!(call.arg),
        }
    }
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

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Unit => write!(f, "()"),
            ExprKind::Lit(lit) => write!(f, "{}", lit),
            ExprKind::Path(path) => write!(f, "{}", pr_display(&path.0)),
            ExprKind::Block(block) => write!(f, "{}", pr_display(block)),
            ExprKind::Infix(infix) => write!(f, "{}", infix),
            ExprKind::Prefix(prefix) => write!(f, "{}", prefix),
            ExprKind::Lambda(lambda) => write!(f, "{}", lambda),
            ExprKind::Call(call) => write!(f, "{}", call),
            ExprKind::Let(block) => write!(f, "{}", pr_display(block)),
            ExprKind::Ty(ty_expr) => write!(f, "{}", ty_expr),
        }
    }
}

impl NodeKindStr for ExprKind {
    fn kind_str(&self) -> String {
        match self {
            ExprKind::Unit => "unit expression ()",
            ExprKind::Lit(_) => "literal",
            ExprKind::Block(_) => "block expression",
            ExprKind::Path(_) => "path",
            ExprKind::Lambda(_) => "lambda",
            ExprKind::Call(_) => "function call",
            ExprKind::Let(_) => "let expression",
            ExprKind::Ty(_) => "type ascription",
            ExprKind::Infix(_) => "infix expression",
            ExprKind::Prefix(_) => "prefix expression",
        }
        .to_string()
    }
}
