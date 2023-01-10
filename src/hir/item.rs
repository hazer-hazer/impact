use crate::{
    resolve::def::DefId,
    span::span::{Ident, Span, WithSpan},
};

use super::{expr::Expr, ty::Ty, N};

#[derive(Debug, Clone, Copy)]
pub struct ItemId(DefId);

impl ItemId {
    pub fn new(def_id: DefId) -> Self {
        ItemId(def_id)
    }

    pub fn as_usize(&self) -> usize {
        self.0.as_usize()
    }
}

impl Into<DefId> for ItemId {
    fn into(self) -> DefId {
        self.0
    }
}

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
    def_id: DefId,
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(def_id: DefId, kind: ItemKind, span: Span) -> Self {
        Self { def_id, kind, span }
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
