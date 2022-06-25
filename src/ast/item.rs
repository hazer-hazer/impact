use crate::span::span::{Ident, Span, WithSpan};

use super::{ty::Ty, NodeId, N, PR};

pub enum ItemKind {
    Type(PR<Ident>, PR<N<Ty>>),
    Mod(PR<Ident>, Vec<PR<N<Item>>>),
}

pub struct Item {
    id: NodeId,
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(id: NodeId, kind: ItemKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn name(&self) -> Option<&Ident> {
        match self.kind() {
            ItemKind::Type(name, _) | ItemKind::Mod(name, _) => Some(name.as_ref().unwrap()),
        }
    }

    pub fn id(&self) -> NodeId {
        self.id
    }
}

impl WithSpan for Item {
    fn span(&self) -> Span {
        self.span
    }
}
