use std::fmt::Display;

use crate::span::span::{Span, WithSpan};

use super::{expr::Expr, item::Item, pr_display, pr_node_kind_str, NodeId, NodeKindStr, N, PR};

pub struct Stmt {
    id: NodeId,
    kind: StmtKind,
    span: Span,
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

impl Stmt {
    pub fn new(id: NodeId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }

    pub fn id(&self) -> NodeId {
        self.id
    }
}

impl WithSpan for Stmt {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum StmtKind {
    Expr(PR<N<Expr>>),
    Item(PR<N<Item>>),
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtKind::Expr(expr) => write!(f, "{}", pr_display(expr)),
            StmtKind::Item(item) => write!(f, "{}", pr_display(item)),
        }
    }
}

impl NodeKindStr for StmtKind {
    fn kind_str(&self) -> String {
        match self {
            StmtKind::Expr(expr) => pr_node_kind_str(expr),
            StmtKind::Item(item) => pr_node_kind_str(item),
        }
    }
}
