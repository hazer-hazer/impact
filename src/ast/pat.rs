use std::fmt::Display;

use crate::span::span::{Ident, Span, WithSpan};

use super::{pr_display, NodeId, WithNodeId, PR};

#[derive(Debug)]
pub enum PatKind {
    // TODO: Lit
    Ident(PR<Ident>),
}

impl Display for PatKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatKind::Ident(ident) => write!(f, "{}", pr_display(ident)),
        }
    }
}

#[derive(Debug)]
pub struct Pat {
    id: NodeId,
    kind: PatKind,
    span: Span,
}

impl Pat {
    pub fn new(id: NodeId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &PatKind {
        &self.kind
    }
}

impl WithSpan for Pat {
    fn span(&self) -> Span {
        self.span
    }
}

impl WithNodeId for Pat {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}
