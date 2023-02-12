use std::fmt::Display;

use crate::{
    parser::token::{FloatKind, IntKind},
    span::span::{Span, Symbol, WithSpan},
};

use super::{
    is_block_ended, pat::Pat, pr_display, stmt::Stmt, ty::Ty, IsBlockEnded, NodeId, NodeKindStr,
    Path, WithNodeId, N, PR,
};

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
}

impl IsBlockEnded for Expr {
    fn is_block_ended(&self) -> bool {
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

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(val) => write!(f, "{}", if *val { "true" } else { "false" }),
            Self::Int(val, kind) => write!(f, "{}{}", val, kind),
            Self::Float(val, kind) => write!(f, "{}{}", val, kind),
            Self::String(val) => write!(f, "\"{}\"", val),
        }
    }
}

// #[derive(Debug, Clone, Copy)]
// pub struct InfixOp {
//     id: NodeId,
//     symbol: Ident,
//     span: Span,
// }

// impl InfixOp {
//     pub fn from_tok(id: NodeId, tok: Token) -> Self {
//         Self {
//             id,
//             symbol: Ident::new(
//                 tok.span(),
//                 match tok.kind {
//                     TokenKind::Op(op) => Symbol::intern(&op.to_string()),
//                     TokenKind::CustomOp(op) => op,
//                     _ => panic!(),
//                 },
//             ),
//             span: tok.span,
//         }
//     }

//     pub fn symbol(&self) -> Ident {
//         self.symbol
//     }
// }

// impl Display for InfixOp {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.symbol.fmt(f)
//     }
// }

// #[derive(Clone, Copy, Debug)]
// pub enum InfixOpKind {
//     Plus,
//     Minus,
//     Mul,
//     Div,
//     Mod,
// }

// impl InfixOpKind {
//     pub fn from_tok(tok: Token) -> Self {
//         match tok.kind {
//             TokenKind::Infix(infix) => match infix {
//                 token::Infix::Plus => InfixOpKind::Plus,
//                 token::Infix::Minus => InfixOpKind::Minus,
//                 token::Infix::Mul => InfixOpKind::Mul,
//                 token::Infix::Div => InfixOpKind::Div,
//                 token::Infix::Mod => InfixOpKind::Mod,
//             },
//             _ => panic!("Cannot make InfixOpKind from not a Infix Token"),
//         }
//     }
// }

// impl Display for InfixOpKind {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             match self {
//                 Self::Plus => "+",
//                 Self::Minus => "-",
//                 Self::Mul => "*",
//                 Self::Div => "/",
//                 Self::Mod => "%",
//             }
//         )
//     }
// }

#[derive(Debug)]
pub struct Infix {
    pub lhs: PR<N<Expr>>,
    pub op: PathExpr,
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

impl Display for PathExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pr_display(&self.0))
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
    Lit(Lit),
    Paren(PR<Box<Expr>>),
    Path(PathExpr),
    Block(PR<Block>),
    Infix(Infix),
    Lambda(Lambda),
    Call(Call),
    Let(PR<Block>),
    Ty(TyExpr),
}

impl IsBlockEnded for ExprKind {
    fn is_block_ended(&self) -> bool {
        match self {
            Self::Lit(_) | Self::Path(_) | Self::Ty(_) | Self::Paren(_) => false,
            Self::Block(_) | Self::Let(_) => true,
            Self::Infix(infix) => is_block_ended!(infix.rhs),
            Self::Lambda(lambda) => is_block_ended!(lambda.body),
            Self::Call(call) => is_block_ended!(call.arg),
        }
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lit(lit) => write!(f, "{}", lit),
            Self::Paren(expr) => write!(f, "({})", pr_display(expr)),
            Self::Path(path) => write!(f, "{}", path),
            Self::Block(block) => write!(f, "{}", pr_display(block)),
            Self::Infix(infix) => write!(f, "{}", infix),
            Self::Lambda(lambda) => write!(f, "{}", lambda),
            Self::Call(call) => write!(f, "{}", call),
            Self::Let(block) => write!(f, "{}", pr_display(block)),
            Self::Ty(ty_expr) => write!(f, "{}", ty_expr),
        }
    }
}

impl NodeKindStr for ExprKind {
    fn kind_str(&self) -> String {
        match self {
            Self::Lit(_) => "literal",
            Self::Paren(_) => "parenthesized expression", // FIXME: Inner expression `kind_str`
            Self::Block(_) => "block expression",
            Self::Path(_) => "path",
            Self::Lambda(_) => "lambda",
            Self::Call(_) => "function call",
            Self::Let(_) => "let expression",
            Self::Ty(_) => "type ascription",
            Self::Infix(_) => "infix expression",
        }
        .to_string()
    }
}
