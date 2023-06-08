use std::fmt::Display;

use super::{
    expr::Expr, impl_with_node_id, item::Item, pat::Pat, pr_display, pr_node_kind_str, NodeId,
    NodeKindStr, WithNodeId, N, PR,
};
use crate::span::{impl_with_span, Span, WithSpan};

#[derive(Debug)]
pub struct Stmt {
    id: NodeId,
    kind: StmtKind,
    span: Span,
}

impl_with_node_id!(Stmt);
impl_with_span!(Stmt);

impl Stmt {
    pub fn new(id: NodeId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl NodeKindStr for Stmt {
    fn kind_str(&self) -> String {
        self.kind().kind_str()
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(PR<N<Expr>>),
    Item(PR<N<Item>>),
    Local(PR<N<Pat>>, PR<N<Expr>>),
}

impl WithSpan for StmtKind {
    fn span(&self) -> Span {
        match self {
            StmtKind::Expr(expr) => expr.span(),
            StmtKind::Item(item) => item.span(),
            StmtKind::Local(pat, expr) => pat.span().to(expr.span()),
        }
    }
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtKind::Expr(expr) => write!(f, "{}", pr_display(expr)),
            StmtKind::Item(item) => write!(f, "{}", pr_display(item)),
            StmtKind::Local(pat, expr) => {
                write!(f, "let {} = {}", pr_display(pat), pr_display(expr))
            },
        }
    }
}

impl NodeKindStr for StmtKind {
    fn kind_str(&self) -> String {
        match self {
            StmtKind::Expr(expr) => pr_node_kind_str(expr),
            StmtKind::Item(item) => pr_node_kind_str(item),
            StmtKind::Local(_pat, _expr) => "let statement".to_string(),
        }
    }
}
