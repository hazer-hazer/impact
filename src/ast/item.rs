use std::fmt::Display;

use super::{
    expr::Expr, is_block_ended, pat::Pat, pr_display, prs_display_join, ty::Ty, IsBlockEnded,
    NodeId, NodeKindStr, WithNodeId, N, PR,
};
use crate::span::{
    impl_with_span,
    sym::{Ident, Internable},
    Span, WithSpan,
};
#[derive(Debug)]
pub struct ExternItem {
    id: NodeId,
    pub name: PR<Ident>,
    pub ty: PR<N<Ty>>,
    span: Span,
}

impl ExternItem {
    pub fn new(id: NodeId, name: PR<Ident>, ty: PR<N<Ty>>, span: Span) -> Self {
        Self { id, name, ty, span }
    }
}

impl WithNodeId for ExternItem {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl_with_span!(ExternItem);

impl Display for ExternItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", pr_display(&self.name), pr_display(&self.ty))
    }
}

#[derive(Debug)]
pub struct Field {
    pub id: NodeId,
    pub index: usize,
    pub name: Option<PR<Ident>>,
    pub ty: PR<N<Ty>>,
    span: Span,
}

impl_with_span!(Field);

impl Field {
    pub fn new(
        id: NodeId,
        index: usize,
        name: Option<PR<Ident>>,
        ty: PR<N<Ty>>,
        span: Span,
    ) -> Self {
        Self {
            id,
            index,
            name,
            ty,
            span,
        }
    }

    /// Get accessor function name.
    /// For named field it is its name.
    /// For unnamed field name is its index.
    /// Note: Panics on error node
    pub fn accessor_name(&self) -> Ident {
        self.name
            .as_ref()
            .map(|name| name.clone().unwrap())
            .unwrap_or_else(|| Ident::new(self.span(), self.index.to_string().intern()))
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.name
                .as_ref()
                .map_or("".to_string(), |name| format!("{}: ", pr_display(name))),
            pr_display(&self.ty)
        )
    }
}

#[derive(Debug)]
pub struct Variant {
    pub id: NodeId,
    pub ctor_id: NodeId,
    pub name: PR<Ident>,
    pub fields: Vec<PR<Field>>,
    span: Span,
}

impl WithNodeId for Variant {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl Variant {
    pub fn new(
        id: NodeId,
        ctor_id: NodeId,
        name: PR<Ident>,
        fields: Vec<PR<Field>>,
        span: Span,
    ) -> Self {
        Self {
            id,
            ctor_id,
            name,
            fields,
            span,
        }
    }
}

impl_with_span!(Variant);

impl Display for Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            pr_display(&self.name),
            prs_display_join(&self.fields, " ")
        )
    }
}

#[derive(Debug)]
pub enum ItemKind {
    Type(PR<Ident>, GenericParams, PR<N<Ty>>),
    Mod(PR<Ident>, Vec<PR<N<Item>>>),
    Decl(PR<Ident>, Vec<PR<Pat>>, PR<N<Expr>>),
    Adt(PR<Ident>, GenericParams, Vec<PR<Variant>>),
    Extern(Vec<PR<ExternItem>>),
}

impl Display for ItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemKind::Type(name, generics, ty) => {
                write!(
                    f,
                    "type {} {} = {}",
                    pr_display(name),
                    generics,
                    pr_display(ty)
                )
            },
            ItemKind::Mod(name, items) => write!(
                f,
                "mod {} {{{}}}",
                pr_display(name),
                items
                    .iter()
                    .map(|item| pr_display(item))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            ItemKind::Decl(name, params, body) => write!(
                f,
                "{} {} = {}",
                pr_display(name),
                params
                    .iter()
                    .map(|param| pr_display(param))
                    .collect::<Vec<_>>()
                    .join(" "),
                pr_display(body)
            ),
            ItemKind::Adt(name, generics, variants) => {
                write!(
                    f,
                    "data {} {} = {}",
                    pr_display(name),
                    generics,
                    prs_display_join(variants, " | ")
                )
            },
            ItemKind::Extern(items) => write!(f, "extern {{{}}}", prs_display_join(items, ", ")),
        }
    }
}

impl NodeKindStr for ItemKind {
    fn kind_str(&self) -> String {
        match self {
            ItemKind::Type(name, ..) => format!("type alias `{}`", pr_display(name)),
            ItemKind::Mod(name, _) => format!("module `{}`", pr_display(name)),
            ItemKind::Decl(name, params, _) if params.is_empty() => {
                format!("value `{}`", pr_display(name))
            },
            ItemKind::Decl(name, ..) => format!("function `{}`", pr_display(name)),
            ItemKind::Adt(name, ..) => format!("data type `{}`", pr_display(name)),
            ItemKind::Extern(_) => format!("extern block"),
        }
    }
}

#[derive(Debug)]
pub struct TyParam {
    id: NodeId,
    pub name: PR<Ident>,
}

impl TyParam {
    pub fn new(id: NodeId, name: PR<Ident>) -> Self {
        Self { id, name }
    }
}

impl WithNodeId for TyParam {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl Display for TyParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pr_display(&self.name))
    }
}

#[derive(Debug)]
pub struct GenericParams {
    pub ty_params: Vec<PR<TyParam>>,
}

impl GenericParams {
    pub fn new(ty_params: Vec<PR<TyParam>>) -> Self {
        Self { ty_params }
    }
}

impl Display for GenericParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ty_params
            .iter()
            .map(pr_display)
            .collect::<Vec<_>>()
            .join(" ")
            .fmt(f)
    }
}

#[derive(Debug)]
pub struct Item {
    id: NodeId,
    kind: ItemKind,
    span: Span,
    is_block_ended: bool,
}

impl Item {
    pub fn new(id: NodeId, kind: ItemKind, span: Span, is_block_ended: bool) -> Self {
        Self {
            id,
            kind,
            span,
            is_block_ended,
        }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn name(&self) -> Option<&Ident> {
        match self.kind() {
            ItemKind::Type(name, ..)
            | ItemKind::Mod(name, _)
            | ItemKind::Decl(name, ..)
            | ItemKind::Adt(name, ..) => {
                Some(name.as_ref().expect("Error Ident node in `Item::name`"))
            },
            ItemKind::Extern(_) => None,
        }
    }

    pub fn is_var(&self) -> bool {
        match self.kind() {
            ItemKind::Decl(_, params, _) => params.is_empty(),
            _ => false,
        }
    }
}

impl IsBlockEnded for Item {
    fn is_block_ended(&self) -> bool {
        self.is_block_ended
            || match &self.kind {
                ItemKind::Type(..) => false,
                ItemKind::Adt(..) => false,
                ItemKind::Mod(..) | ItemKind::Extern(_) => true,
                ItemKind::Decl(_, _, body) => is_block_ended!(body),
            }
    }
}

impl WithNodeId for Item {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl NodeKindStr for Item {
    fn kind_str(&self) -> String {
        self.kind().kind_str()
    }
}

impl_with_span!(Item);
