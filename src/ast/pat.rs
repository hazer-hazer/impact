use std::fmt::Display;

use super::{pr_display, NodeId, WithNodeId, PR};
use crate::span::{impl_with_span, sym::Ident, Span, WithSpan};

#[derive(Debug)]
pub enum PatKind {
    // TODO: Lit

    // TODO: Replace Unit variant with Path pattern which refers to builtin type `()`
    Unit,
    Ident(PR<Ident>),
}

impl Display for PatKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatKind::Unit => "()".fmt(f),
            PatKind::Ident(ident) => pr_display(ident).fmt(f),
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

impl_with_span!(Pat);

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
