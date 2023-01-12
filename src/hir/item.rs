use crate::{
    ast::NodeId,
    cli::color::Color,
    cli::color::Colorize,
    dt::idx::declare_idx,
    resolve::def::DefId,
    span::span::{Ident, Span, WithSpan},
};

use super::{expr::Expr, ty::Ty, N};

declare_idx!(ItemId, DefId, "#", Color::Yellow);

pub struct TypeItem {
    pub name: Ident,
    pub ty: N<Ty>,
}

pub struct Mod {
    pub name: Ident,
    pub items: Vec<ItemId>,
}

pub struct Decl {
    pub name: Ident,
    pub value: N<Expr>,
}

pub enum ItemKind {
    Type(TypeItem),
    Mod(Mod),
    Decl(Decl),
}

pub struct Item {
    node_id: NodeId,
    def_id: DefId,
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(node_id: NodeId, def_id: DefId, kind: ItemKind, span: Span) -> Self {
        Self {
            node_id,
            def_id,
            kind,
            span,
        }
    }

    pub fn name(&self) -> Ident {
        match self.kind() {
            ItemKind::Type(t) => t.name,
            ItemKind::Mod(m) => m.name,
            ItemKind::Decl(d) => d.name,
        }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn def_id(&self) -> DefId {
        self.def_id
    }
}

impl WithSpan for Item {
    fn span(&self) -> Span {
        self.span
    }
}
