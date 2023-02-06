use crate::{
    cli::color::Color,
    cli::color::Colorize,
    dt::idx::declare_idx,
    resolve::def::DefId,
    span::span::{Ident, Span, WithSpan},
};

use super::{expr::Expr, ty::Ty, OwnerId};

declare_idx!(ItemId, OwnerId, "item{}", Color::Yellow);

#[derive(Debug)]
pub struct TypeItem {
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Mod {
    pub items: Vec<ItemId>,
}

#[derive(Debug)]
pub struct Decl {
    pub value: Expr,
}

#[derive(Debug)]
pub enum ItemKind {
    Type(TypeItem),
    Mod(Mod),
    Decl(Decl),
}

#[derive(Debug)]
pub struct ItemNode {
    name: Ident,
    owner_id: OwnerId,
    kind: ItemKind,
    span: Span,
}

impl ItemNode {
    pub fn new(name: Ident, def_id: DefId, kind: ItemKind, span: Span) -> Self {
        Self {
            name,
            owner_id: OwnerId(def_id),
            kind,
            span,
        }
    }

    pub fn name(&self) -> Ident {
        self.name
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

impl WithSpan for ItemNode {
    fn span(&self) -> Span {
        self.span
    }
}
