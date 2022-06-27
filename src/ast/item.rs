use std::fmt::Display;

use crate::span::span::{Ident, Span, WithSpan};

use super::{pr_display, ty::Ty, NodeId, N, PR};

pub struct Item {
    id: NodeId,
    kind: ItemKind,
    span: Span,
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

pub enum ItemKind {
    Type(PR<Ident>, PR<N<Ty>>),
    Mod(PR<Ident>, Vec<PR<N<Item>>>),
}

impl Display for ItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemKind::Type(name, ty) => write!(f, "type {} = {}", pr_display(name), pr_display(ty)),
            ItemKind::Mod(name, items) => write!(
                f,
                "mod {} {{{}}}",
                pr_display(name),
                items
                    .iter()
                    .map(|item| pr_display(item))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        }
    }
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
