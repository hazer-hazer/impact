use crate::{
    ast::NodeId,
    span::span::{Span, WithSpan},
};

use super::{expr::Expr, item::ItemId, N};

pub struct Stmt {
    node_id: NodeId,
    kind: StmtKind,
    span: Span,
}

impl Stmt {
    pub fn new(node_id: NodeId, kind: StmtKind, span: Span) -> Self {
        Self {
            node_id,
            kind,
            span,
        }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }
}

impl WithSpan for Stmt {
    fn span(&self) -> Span {
        self.span
    }
}

pub enum StmtKind {
    Expr(N<Expr>),
    Item(ItemId),
}
