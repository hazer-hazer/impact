use std::fmt::Display;

use crate::span::span::{Ident, Span, WithSpan};

use super::{expr::Expr, pr_display, ty::Ty, NodeId, NodeKindStr, WithNodeId, N, PR};

#[derive(Debug)]
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
            ItemKind::Type(name, _) | ItemKind::Mod(name, _) | ItemKind::Decl(name, _, _) => {
                Some(name.as_ref().unwrap())
            },
        }
    }

    pub fn is_var(&self) -> bool {
        match self.kind() {
            ItemKind::Decl(_, params, _) => params.is_empty(),
            _ => false,
        }
    }
}

impl WithNodeId for Item {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl WithSpan for Item {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl NodeKindStr for Item {
    fn kind_str(&self) -> String {
        self.kind().kind_str()
    }
}

#[derive(Debug)]
pub enum ItemKind {
    Type(PR<Ident>, PR<N<Ty>>),
    Mod(PR<Ident>, Vec<PR<N<Item>>>),
    Decl(PR<Ident>, Vec<PR<Ident>>, PR<N<Expr>>),
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
            ItemKind::Decl(name, params, body) => write!(
                f,
                "{} {} = {}",
                pr_display(name),
                params
                    .iter()
                    .map(|param| pr_display(param))
                    .collect::<Vec<_>>()
                    .join(" "),
                pr_display(body)
            ),
        }
    }
}

impl NodeKindStr for ItemKind {
    fn kind_str(&self) -> String {
        match self {
            ItemKind::Type(name, _) => format!("type alias {}", pr_display(name)),
            ItemKind::Mod(name, _) => format!("module {}", pr_display(name)),
            ItemKind::Decl(name, _, _) => format!("{} declaration", pr_display(name)),
        }
    }
}
