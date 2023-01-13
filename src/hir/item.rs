use crate::{
    ast::NodeId,
    cli::color::Color,
    cli::color::Colorize,
    dt::idx::declare_idx,
    resolve::def::DefId,
    span::span::{Ident, Span, WithSpan},
};

use super::{expr::Expr, ty::Ty, OwnerId, N};

declare_idx!(ItemId, OwnerId, "item{}", Color::Yellow);

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
    owner_id: OwnerId,
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(owner_id: OwnerId, kind: ItemKind, span: Span) -> Self {
        Self {
            owner_id,
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
        self.owner_id.into()
    }

    pub fn owner_id(&self) -> OwnerId {
        self.owner_id
    }
}

impl WithSpan for Item {
    fn span(&self) -> Span {
        self.span
    }
}
