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
pub struct TyAlias {
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
    TyAlias(TyAlias),
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

macro_rules! specific_item_nodes {
    ($($name: ident $method_name: ident $ty: tt),*) => {
        // $(
        //     #[derive(Debug)]
        //     pub struct $name {
        //         name: Ident,
        //         owner_id: OwnerId,
        //         kind: $ty,
        //         span: Span
        //     }

        // )*

        // $(
        //     impl $name {
        //         pub fn name(&self) -> Ident {
        //             self.name
        //         }

        //         pub fn def_id(&self) -> DefId {
        //             self.owner_id.into()
        //         }

        //         pub fn owner_id(&self) -> OwnerId {
        //             self.owner_id
        //         }
        //     }
        // )*

        $(
            impl ItemNode {
                pub fn $method_name(&self) -> &$ty {
                    match self.kind() {
                        ItemKind::$ty(inner) => inner,
                        _ => unreachable!()
                    }
                }
            }
        )*
    };
}

specific_item_nodes!(
    TyAliasNode ty_alias TyAlias,
    ModNode mod_ Mod,
    DeclNode decl Decl
);
