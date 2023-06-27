use std::fmt::Display;

use super::{impl_with_node_id, pr_display, NodeId, WithNodeId, PR, IdentNode};
use crate::span::{impl_with_span, sym::Ident, Span, WithSpan};

#[derive(Debug)]
pub enum PatKind {
    // TODO: Lit

    // TODO: Replace Unit variant with Path pattern which refers to builtin type `()`
    Unit,
    Ident(PR<IdentNode>),
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

impl_with_span!(Pat);
impl_with_node_id!(Pat);

impl Pat {
    pub fn new(id: NodeId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &PatKind {
        &self.kind
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}
