use super::{impl_with_hir_id, BodyId, HirId, OwnerId, Ty, Variant, WithHirId, ROOT_OWNER_ID};
use crate::{
    cli::color::Color,
    dt::idx::declare_idx,
    resolve::def::DefId,
    span::{impl_with_span, sym::Ident, Span, WithSpan},
};

declare_idx!(ItemId, OwnerId, "[item{}]", Color::Yellow);
pub const ROOT_ITEM_ID: ItemId = ItemId(ROOT_OWNER_ID);

impl ItemId {
    pub fn hir_id(&self) -> HirId {
        HirId::new_owner(self.inner().into())
    }

    pub fn def_id(&self) -> DefId {
        self.inner().inner()
    }
}

impl Into<HirId> for ItemId {
    fn into(self) -> HirId {
        self.hir_id()
    }
}

#[derive(Debug)]
pub struct TyParam {
    pub id: HirId,
    pub def_id: DefId,
    pub name: Ident,
}

impl_with_hir_id!(TyParam);

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
    pub span: Span,
}

impl_with_span!(Mod);

#[derive(Debug)]
pub struct Func {
    body: BodyId,
}

#[derive(Debug)]
pub struct Field {
    pub id: HirId,
    /// Accessor function definition, None for ADT, Some for structures.
    pub accessor_def_id: Option<DefId>,
    pub name: Option<Ident>,
    pub ty: Ty,
    pub span: Span,
}

impl_with_span!(Field);
impl_with_hir_id!(Field);

#[derive(Debug)]
pub struct VariantNode {
    pub id: HirId,
    pub def_id: DefId,
    pub ctor_def_id: DefId,
    pub name: Ident,
    pub fields: Vec<Field>,
    pub span: Span,
}

impl_with_span!(VariantNode);
impl_with_hir_id!(VariantNode);

#[derive(Debug)]
pub struct Adt {
    pub generics: GenericParams,
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Struct {
    pub ctor_def_id: DefId,
    pub generics: GenericParams,
    pub fields: Vec<Field>,
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
    Struct(Struct),
    ExternItem(ExternItem),
}

#[derive(Debug)]
pub struct ItemNode {
    name: Ident,
    owner_id: OwnerId,
    pub kind: ItemKind,
    span: Span,
}

impl_with_span!(ItemNode);

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
    Struct struct_ Struct;
    ExternItem extern_item ExternItem;
);
