use std::fmt::Display;

use crate::span::span::{Ident, Span, WithSpan};

use super::{
    expr::Expr, is_block_ended, pat::Pat, pr_display, prs_display_join, ty::Ty, IsBlockEnded,
    NodeId, NodeKindStr, WithNodeId, N, PR,
};

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
                Some(name.as_ref().expect("Error Ident node in `Item::name`"))
            },
            ItemKind::Extern(_) => None,
        }
    }

    pub fn is_var(&self) -> bool {
        match self.kind() {
            ItemKind::Decl(_, params, _) => params.is_empty(),
            _ => false,
        }
    }
}

impl IsBlockEnded for Item {
    fn is_block_ended(&self) -> bool {
        self.kind.is_block_ended()
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
pub struct ExternItem {
    id: NodeId,
    pub name: PR<Ident>,
    pub ty: PR<N<Ty>>,
    span: Span,
}

impl ExternItem {
    pub fn new(id: NodeId, name: PR<Ident>, ty: PR<N<Ty>>, span: Span) -> Self {
        Self { id, name, ty, span }
    }
}

impl WithNodeId for ExternItem {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl WithSpan for ExternItem {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for ExternItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", pr_display(&self.name), pr_display(&self.ty))
    }
}

#[derive(Debug)]
pub enum ItemKind {
    Type(PR<Ident>, PR<N<Ty>>),
    Mod(PR<Ident>, Vec<PR<N<Item>>>),
    Decl(PR<Ident>, Vec<PR<Pat>>, PR<N<Expr>>),
    Extern(Vec<PR<ExternItem>>),
}

impl IsBlockEnded for ItemKind {
    fn is_block_ended(&self) -> bool {
        match self {
            ItemKind::Type(_, _) => false, // FIXME: Always?
            ItemKind::Mod(_, _) | ItemKind::Extern(_) => true,
            ItemKind::Decl(_, _, body) => is_block_ended!(body),
        }
    }
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
            ItemKind::Extern(items) => write!(f, "extern {{{}}}", prs_display_join(items, ", ")),
        }
    }
}

impl NodeKindStr for ItemKind {
    fn kind_str(&self) -> String {
        match self {
            ItemKind::Type(name, _) => format!("type alias {}", pr_display(name)),
            ItemKind::Mod(name, _) => format!("module {}", pr_display(name)),
            ItemKind::Decl(name, _, _) => format!("{} declaration", pr_display(name)),
            ItemKind::Extern(_) => format!("extern block"),
        }
    }
}
