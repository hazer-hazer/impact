use crate::{
    ast::NodeId,
    span::span::{Ident, Span, WithSpan},
};

pub enum PatKind {
    Ident(Ident),
}

pub struct Pat {
    node_id: NodeId,
    kind: PatKind,
    span: Span,
}

impl Pat {
    pub fn new(node_id: NodeId, kind: PatKind, span: Span) -> Self {
        Self {
            node_id,
            kind,
            span,
        }
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
