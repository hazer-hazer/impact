use crate::span::span::{Span, WithSpan};

use super::{expr::Expr, item::Item, NodeId, N, PR};

pub struct Stmt {
    id: NodeId,
    kind: StmtKind,
    span: Span,
}

impl Stmt {
    pub fn new(id: NodeId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
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
    Expr(PR<N<Expr>>),
    Item(PR<N<Item>>),
}

// impl Display for StmtKind {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             StmtKind::Expr(expr) => write!(f, "{}", expr),
//         }
//     }
// }
