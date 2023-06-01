use std::fmt::Display;

use super::{
    impl_with_node_id, is_block_ended, pat::Pat, pr_display, stmt::Stmt, ty::Ty, IdentNode,
    IsBlockEnded, NodeId, NodeKindStr, Path, WithNodeId, N, PR,
};
use crate::{
    ast::prs_display_join,
    parser::token::{FloatKind, IntKind},
    span::{impl_with_span, sym::Symbol, Span, WithSpan},
};

#[derive(Debug)]
pub struct Arm {
    pub pat: PR<Pat>,
    pub body: PR<N<Expr>>,
    pub span: Span,
}

impl_with_span!(Arm);

impl Display for Arm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "  {} => {}\n",
            pr_display(&self.pat),
            pr_display(&self.body)
        )
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(Lit),
    Paren(PR<N<Expr>>),
    Path(PathExpr),
    Block(PR<Block>),
    Infix(Infix),
    Lambda(Lambda),
    Call(Call),
    Let(PR<Block>),
    Ty(TyExpr),
    DotOp(PR<N<Expr>>, PR<IdentNode>),
    Match(PR<N<Expr>>, Vec<PR<Arm>>),
}

impl IsBlockEnded for ExprKind {
    fn is_block_ended(&self) -> bool {
        match self {
            Self::DotOp(..) | Self::Lit(_) | Self::Path(_) | Self::Ty(_) | Self::Paren(_) => false,
            Self::Block(_) | Self::Let(_) => true,
            Self::Infix(infix) => is_block_ended!(infix.rhs),
            Self::Lambda(lambda) => is_block_ended!(lambda.body),
            Self::Call(call) => call.args.iter().any(|arg| is_block_ended!(arg)),
            // Self::Match(_, arms) => arms.last().as_ref().map_or(false, |arm| {
            //     arm.as_ref().map_or(false, |arm| {
            //         arm.body
            //             .as_ref()
            //             .map_or(false, |body| body.is_block_ended())
            //     })
            // }),
            Self::Match(..) => true,
        }
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lit(lit) => write!(f, "{lit}"),
            Self::Paren(expr) => write!(f, "({})", pr_display(expr)),
            Self::Path(path) => write!(f, "{path}"),
            Self::Block(block) => write!(f, "{}", pr_display(block)),
            Self::Infix(infix) => write!(f, "{infix}"),
            Self::Lambda(lambda) => write!(f, "{lambda}"),
            Self::Call(call) => write!(f, "{call}"),
            Self::Let(block) => write!(f, "{}", pr_display(block)),
            Self::Ty(ty_expr) => write!(f, "{ty_expr}"),
            Self::DotOp(expr, field) => {
                write!(f, "{}.{}", pr_display(expr), pr_display(field))
            },
            Self::Match(subject, arms) => write!(
                f,
                "match {}\n{}",
                pr_display(subject),
                arms.iter().map(pr_display).collect::<String>()
            ),
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
            Self::DotOp(..) => "member access",
            Self::Match(..) => "match expression",
        }
        .to_string()
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
            Self::Bool(val) => write!(f, "{val}"),
            Self::Int(val, kind) => write!(f, "{val}{kind}"),
            Self::Float(val, kind) => write!(f, "{val}{kind}"),
            Self::String(val) => write!(f, "\"{val}\""),
        }
    }
}

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

impl_with_node_id!(Block);
impl_with_span!(Block);

impl Block {
    pub fn new(id: NodeId, stmts: Vec<PR<N<Stmt>>>, span: Span) -> Self {
        Self { id, stmts, span }
    }

    pub fn stmts(&self) -> &[PR<N<Stmt>>] {
        self.stmts.as_ref()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", prs_display_join(&self.stmts, "\n"))
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
    pub params: Vec<PR<Pat>>,
    pub body: PR<N<Expr>>,
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            prs_display_join(&self.params, " "),
            pr_display(&self.body)
        )
    }
}

#[derive(Debug)]
pub struct Call {
    pub lhs: PR<N<Expr>>,
    pub args: Vec<PR<N<Expr>>>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            pr_display(&self.lhs),
            prs_display_join(&self.args, " ")
        )
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
pub struct Expr {
    id: NodeId,
    kind: ExprKind,
    span: Span,
}

impl_with_node_id!(Expr);
impl_with_span!(Expr);

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

impl NodeKindStr for Expr {
    fn kind_str(&self) -> String {
        self.kind().kind_str()
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}
