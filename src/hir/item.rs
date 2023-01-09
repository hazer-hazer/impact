use crate::{
    resolve::def::DefId,
    span::span::{Ident, Span, WithSpan},
};

use super::{expr::Expr, ty::Ty};

#[derive(Debug, Clone, Copy)]
pub struct ItemId(DefId);

impl ItemId {
    pub fn new(def_id: DefId) -> Self {
        ItemId(def_id)
    }
}

impl Into<DefId> for ItemId {
    fn into(self) -> DefId {
        self.0
    }
}

pub struct TypeItem<'hir> {
    pub name: Ident,
    pub ty: &'hir Ty<'hir>,
}

pub struct Mod<'hir> {
    pub name: Ident,
    pub items: &'hir [&'hir Item<'hir>],
}

pub struct Decl<'hir> {
    pub name: Ident,
    pub value: &'hir Expr<'hir>,
}

pub enum ItemKind<'hir> {
    Type(TypeItem<'hir>),
    Mod(Mod<'hir>),
    Decl(Decl<'hir>),
}

pub struct Item<'hir> {
    def_id: DefId,
    kind: ItemKind<'hir>,
    span: Span,
}

impl<'hir> Item<'hir> {
    pub fn new(def_id: DefId, kind: ItemKind<'hir>, span: Span) -> Self {
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
}

impl<'hir> WithSpan for Item<'hir> {
    fn span(&self) -> Span {
        self.span
    }
}
