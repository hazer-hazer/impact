use crate::span::span::{Ident, Span, WithSpan};

use super::{expr::Expr, ty::Ty, N};

pub struct TypeItem {
    pub name: Ident,
    pub ty: N<Ty>,
}

pub struct Mod {
    pub name: Ident,
    pub items: Vec<Item>,
}

pub struct Decl {
    pub name: Ident,
    pub value: Expr,
}

pub enum ItemKind {
    Type(TypeItem),
    Mod(Mod),
    Decl(Decl),
}

pub struct Item {
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(kind: ItemKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }
}

impl WithSpan for Item {
    fn span(&self) -> Span {
        self.span
    }
}
