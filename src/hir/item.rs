use crate::{
    cli::color::Color,
    cli::color::Colorize,
    dt::idx::declare_idx,
    resolve::def::DefId,
    span::span::{impl_with_span, Ident, Span, WithSpan},
};

use super::{ty::Ty, BodyId, HirId, OwnerId, ROOT_OWNER_ID};

declare_idx!(ItemId, OwnerId, "item{}", Color::Yellow);
pub const ROOT_ITEM_ID: ItemId = ItemId(ROOT_OWNER_ID);

impl ItemId {
    pub fn hir_id(&self) -> HirId {
        HirId::new_owner(self.inner().into())
    }

    pub fn def_id(&self) -> DefId {
        self.inner().inner()
    }
}

#[derive(Debug)]
pub struct TyAlias {
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Mod {
    pub items: Vec<ItemId>,
}

#[derive(Debug)]
pub struct Func {
    body: BodyId,
}

#[derive(Debug)]
pub struct Field {
    pub name: Option<Ident>,
    pub ty: Ty,
    span: Span,
}

impl_with_span!(Field);

#[derive(Debug)]
pub struct Variant {
    pub def_id: DefId,
    pub ctor_def_id: DefId,
    pub name: Ident,
    pub fields: Vec<Field>,
    span: Span,
}

impl_with_span!(Variant);

#[derive(Debug)]
pub struct Data {
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct ExternItem {
    pub ty: Ty,
}

#[derive(Debug)]
pub enum ItemKind {
    TyAlias(TyAlias),
    Mod(Mod),
    Value(BodyId),
    Func(BodyId),
    Data(Data),
    ExternItem(ExternItem),
}

#[derive(Debug)]
pub struct ItemNode {
    name: Ident,
    owner_id: OwnerId,
    pub kind: ItemKind,
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

impl_with_span!(ItemNode);

macro_rules! specific_item_nodes {
    ($($variant: ident $method_name: ident $($ty: ty),+;)*) => {
        $(
            impl ItemNode {
                #[allow(unused_parens)]
                pub fn $method_name(&self) -> &($($ty),+) {
                    match self.kind() {
                        ItemKind::$variant(inner) => inner,
                        _ => unreachable!()
                    }
                }
            }
        )*
    };
}

specific_item_nodes!(
    TyAlias ty_alias TyAlias;
    Mod mod_ Mod;
    Value value BodyId;
    Func func BodyId;
    Data data Data;
    ExternItem extern_item ExternItem;
);
