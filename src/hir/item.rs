use super::{ty::Ty, BodyId, HirId, OwnerId, WithHirId, ROOT_OWNER_ID};
use crate::{
    cli::color::{Color, ColorizedStruct},
    dt::idx::declare_idx,
    resolve::def::DefId,
    span::{impl_with_span, sym::Ident, Span, WithSpan},
};

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
pub struct TyParam {
    pub id: HirId,
    pub def_id: DefId,
    pub name: Ident,
}

impl WithHirId for TyParam {
    fn id(&self) -> HirId {
        self.id
    }
}

#[derive(Debug)]
pub struct GenericParams {
    pub ty_params: Vec<TyParam>,
}

#[derive(Debug)]
pub struct TyAlias {
    pub generics: GenericParams,
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
    pub id: HirId,
    pub name: Option<Ident>,
    pub ty: Ty,
    pub span: Span,
}

impl WithHirId for Field {
    fn id(&self) -> HirId {
        self.id
    }
}

impl_with_span!(Field);

pub type Variant = HirId;

#[derive(Debug)]
pub struct VariantNode {
    pub id: HirId,
    pub def_id: DefId,
    pub ctor_def_id: DefId,
    pub name: Ident,
    pub fields: Vec<Field>,
    pub span: Span,
}

impl WithHirId for VariantNode {
    fn id(&self) -> HirId {
        self.id
    }
}

impl_with_span!(VariantNode);

#[derive(Debug)]
pub struct Adt {
    pub generics: GenericParams,
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
    Adt(Adt),
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
    Adt adt Adt;
    ExternItem extern_item ExternItem;
);
