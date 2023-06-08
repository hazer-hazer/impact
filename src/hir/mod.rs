//! HIR is nothing more than just an unwrapped version of AST, i.e. freed of
//! parse results.

use core::panic;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use self::{
    expr::{BlockNode, ExprKind, ExprNode, Lambda},
    item::{ItemId, ItemKind, ItemNode, Mod, VariantNode},
    pat::PatNode,
    stmt::StmtNode,
    ty::TyNode,
};
use crate::{
    cli::color::{Color},
    dt::{
        idx::{declare_idx, Idx, IndexVec},
        new_type::new_type,
    },
    resolve::def::{DefId, DefKind, DefMap, ROOT_DEF_ID},
    span::{
        impl_with_span,
        sym::{Ident, Symbol},
        Span, WithSpan,
    },
    utils::macros::sub_enum_conversion,
};

pub mod expr;
pub mod item;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod visitor;

type N<T> = Box<T>;

declare_idx!(OwnerId, DefId, "{}", Color::BrightCyan);
declare_idx!(wrapper BodyId, HirId, "body{}");
declare_idx!(OwnerChildId, u32, "#{}", Color::Cyan);

pub const ROOT_OWNER_ID: OwnerId = OwnerId(ROOT_DEF_ID);
pub const OWNER_SELF_CHILD_ID: OwnerChildId = OwnerChildId(0);
pub const FIRST_OWNER_CHILD_ID: OwnerChildId = OwnerChildId(1);

type OwnerChildrenMap<T> = IndexVec<OwnerChildId, Option<T>>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct HirId {
    owner: OwnerId,
    id: OwnerChildId,
}

impl HirId {
    pub fn new(owner: OwnerId, id: OwnerChildId) -> Self {
        Self { owner, id }
    }

    pub fn synth(owner: u32, id: u32) -> Self {
        Self {
            owner: OwnerId::new(DefId::new(owner)),
            id: OwnerChildId(id),
        }
    }

    pub fn new_owner(def_id: DefId) -> Self {
        Self {
            owner: OwnerId(def_id),
            id: OwnerChildId(0),
        }
    }

    pub fn as_owner(&self) -> Option<OwnerId> {
        if self.is_owner() {
            Some(self.owner)
        } else {
            None
        }
    }

    pub fn is_owner(&self) -> bool {
        self.id.as_usize() == 0
    }

    pub fn owner(&self) -> OwnerId {
        self.owner
    }

    pub fn child_id(&self) -> OwnerChildId {
        self.id
    }
}

impl Display for HirId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}]", self.owner, self.id)
    }
}

impl WithHirId for HirId {
    fn id(&self) -> HirId {
        *self
    }
}

pub type HirMap<T> = HashMap<HirId, T>;

pub trait WithHirId {
    fn id(&self) -> HirId;
}

macro_rules! impl_with_hir_id {
    ($ty: ident $(<$($generic: ident)+>)?) => {
        impl<$($($generic,)+)?> WithHirId for $ty<$($($generic,)+)?> {
            fn id(&self) -> HirId {
                self.id
            }
        }
    };
}

pub(crate) use impl_with_hir_id;

#[derive(Debug)]
pub struct ErrorNode {
    pub id: HirId,
    pub span: Span,
}

impl_with_span!(ErrorNode);

// HIR Node is any Node in HIR with HirId
macro_rules! hir_nodes {
    // identified by HirId / other
    ($($name: ident $id_name: ident $ty: ty,)* / $($other_name: ident $variant_name: ident $other_ty: ty,)*) => {
        $(
            new_type!($id_name HirId);
        )*

        #[derive(Debug)]
        pub enum Node {
            $(
                $id_name($ty),
            )*
            $(
                $variant_name($other_ty),
            )*
        }

        impl Node {
            pub fn name(&self) -> &str {
                match self {
                    $(Self::$id_name(_) => stringify!($name),)*
                    $(Self::$variant_name(_) => stringify!($other_name),)*
                }
            }

            pub fn span(&self) -> Span {
                match self {
                    $(Self::$id_name(node) => node.span(),)*
                    $(Self::$variant_name(node) => node.span(),)*
                }
            }

            $(
                pub fn $name(&self) -> &$ty {
                    match self {
                        Self::$id_name(inner) => inner,
                        _ => panic!("Expected `{}` HIR node", stringify!($name)),
                    }
                }
            )*

            $(
                pub fn $other_name(&self) -> &$other_ty {
                    match self {
                        Self::$variant_name(inner) => inner,
                        _ => panic!("Expected `{}` HIR node", stringify!($other_name)),
                    }
                }
            )*
        }

        pub trait Map<'hir> {
            $(
                fn $name(self, $name: $id_name) -> &'hir $ty;
            )*

            fn root(self) -> &'hir Mod;

            fn node(self, id: HirId) -> &'hir Node;

            fn item(self, item_id: ItemId) -> &'hir ItemNode;

            fn body(self, id: BodyId) -> &'hir Body;

            fn expect_owner(self, def_id: DefId) -> &'hir Owner;
            fn expect_owner_node(self, def_id: DefId) -> OwnerNode<'hir>;
            fn string_lit_value(self, expr: Expr) -> Option<Symbol>;
            fn owner_body(self, owner_id: OwnerId) -> Option<BodyId>;
            fn body_owner_kind(self, owner_id: OwnerId) -> BodyOwnerKind;
            fn pat_names(self, pat: Pat) -> Option<Vec<Ident>>;
            fn expr_result_span(self, expr_id: Expr) -> Span;
            fn block_result_span(self, block: Block) -> Span;
            fn return_ty_span(self, def_id: DefId) -> Span;
            fn body_return_ty_span(self, def_id: DefId) -> Span;
        }

        impl<'hir> Map<'hir> for &'hir HIR {
            $(
                fn $name(self, $name: $id_name) -> &'hir $ty {
                    match self.node($name.into()) {
                        Node::$id_name(node) => node,
                        n @ _ => panic!("Expected {} HIR node, got {}", stringify!($name), n.name()),
                    }
                }
            )*

            fn root(self) -> &'hir Mod {
                match self.expect_owner_node(ROOT_DEF_ID) {
                    OwnerNode::Root(root) => root,
                    OwnerNode::Item(_) => unreachable!(),
                }
            }

            fn node(self, id: HirId) -> &'hir Node {
                self.owners
                    .get_unwrap(id.owner.inner())
                    .nodes
                    .get_unwrap(id.id)
            }

            fn item(self, item_id: ItemId) -> &'hir ItemNode {
                match self.owners.get_unwrap(item_id.inner().inner()).owner_node() {
                    OwnerNode::Item(item) => item,
                    OwnerNode::Root(_) => todo!(),
                }
            }

            fn body(self, id: BodyId) -> &'hir Body {
                self.owners
                    .get_unwrap(id.0.owner.inner())
                    .bodies
                    .get_unwrap(id.0.id)
            }

            fn expect_owner(self, def_id: DefId) -> &'hir Owner {
                self.owners
                    .get_expect(def_id, &format!("Expect owner {}", def_id))
            }

            fn expect_owner_node(self, def_id: DefId) -> OwnerNode<'hir> {
                self.expect_owner(def_id).owner_node()
            }

            fn string_lit_value(self, expr: Expr) -> Option<Symbol> {
                match self.expr(expr).kind() {
                    ExprKind::Lit(lit) => match lit {
                        &expr::Lit::String(val) => Some(val),
                        _ => None,
                    },
                    _ => None,
                }
            }

            fn owner_body(self, owner_id: OwnerId) -> Option<BodyId> {
                match self.node(HirId::new_owner(owner_id.into())) {
                    Node::Expr(ExprNode {
                        kind: ExprKind::Lambda(Lambda { body_id: body, .. }),
                        ..
                    }) => Some(*body),
                    Node::Item(ItemNode {
                        kind: ItemKind::Func(body) | ItemKind::Value(body),
                        ..
                    }) => Some(*body),
                    _ => None,
                }
            }

            fn body_owner_kind(self, owner_id: OwnerId) -> BodyOwnerKind {
                match self.node(HirId::new_owner(owner_id.into())) {
                    Node::Expr(ExprNode {
                        kind: ExprKind::Lambda(Lambda { .. }),
                        ..
                    }) => BodyOwnerKind::Lambda,
                    Node::Item(ItemNode {
                        kind: ItemKind::Func(_),
                        ..
                    }) => BodyOwnerKind::Func,
                    Node::Item(ItemNode {
                        kind: ItemKind::Value(_),
                        ..
                    }) => BodyOwnerKind::Value,
                    _ => panic!(),
                }
            }

            fn pat_names(self, pat: Pat) -> Option<Vec<Ident>> {
                match self.pat(pat).kind() {
                    pat::PatKind::Unit => None,
                    &pat::PatKind::Ident(name) => Some(vec![name]),
                }
            }

            fn expr_result_span(self, expr_id: Expr) -> Span {
                let expr = self.expr(expr_id);
                match expr.kind() {
                    ExprKind::Lit(_)
                    | ExprKind::Path(_)
                    | ExprKind::Lambda(_)
                    | ExprKind::Call(_)
                    | ExprKind::Builtin(_)
                    | ExprKind::Ty(_) => expr.span(),
                    &ExprKind::Block(block) | &ExprKind::Let(block) => self.block_result_span(block),
                    ExprKind::Match(subject, arms) => arms.first().map_or(self.expr(*subject).span(), |arm| self.expr_result_span(arm.body)),
                }
            }

            fn block_result_span(self, block: Block) -> Span {
                let block = self.block(block);

                block.expr().map_or_else(
                    || {
                        self.stmt(*block.stmts().last().unwrap())
                            .span()
                            .point_after_hi()
                    },
                    |expr| self.expr_result_span(expr),
                )
            }

            /// Assumes def_id points to Func or Lambda
            fn return_ty_span(self, def_id: DefId) -> Span {
                self.pat(
                    self.body(self.owner_body(def_id.into()).unwrap())
                        .params
                        .last()
                        .copied()
                        .unwrap(),
                )
                .span()
                .point_after_hi()
                // match self.node(HirId::new_owner(def_id)) {
                //     Node::ExprNode(expr) => match expr.kind() {
                //         ExprKind::Lambda(Lambda { body }) => todo!(),
                //         _ => panic!(),
                //     },
                //     Node::ItemNode(item) => todo!(),
                //     _ => panic!(),
                // }
            }

            /// Assumes that def_id points to a body owner.
            /// Returns span pointing after parameter of the body with a parameter,
            /// otherwise span pointing after name of the definition.
            fn body_return_ty_span(self, def_id: DefId) -> Span {
                let body = self.body(self.owner_body(def_id.into()).unwrap());
                if let Some(param) = body.params.last().copied() {
                    self.pat(param).span().point_after_hi()
                } else {
                    self.item(ItemId::new(def_id.into()))
                        .name()
                        .span()
                        .point_after_hi()
                }
            }
        }
    };
}

hir_nodes!(
    expr Expr ExprNode,
    stmt Stmt StmtNode,
    pat Pat PatNode,
    block Block BlockNode,
    ty Ty TyNode,
    expr_path ExprPath PathNode<ExprRes>,
    ty_path TyPath PathNode<TyRes>,
    variant Variant VariantNode,
    /
    item Item ItemNode,
    root Root Mod,
    error Error ErrorNode,
);

/// Do not confuse with `Node`, this is just kind of list of names.
// Note: Keep referencing `Node` names
pub enum NodeKind {
    Expr,
    Stmt,
    Pat,
    Block,
    Ty,
    ExprPath,
    TyPath,
    Variant,
    Item,
    Root,
    Error,
}

impl Display for NodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NodeKind::Expr => "expression",
                NodeKind::Stmt => "statement",
                NodeKind::Pat => "pattern",
                NodeKind::Block => "block",
                NodeKind::Ty => "type",
                NodeKind::ExprPath => "expression path",
                NodeKind::TyPath => "type path",
                NodeKind::Variant => "variant",
                NodeKind::Item => "item",
                NodeKind::Root => "root",
                NodeKind::Error => "[ERROR]",
            }
        )
    }
}

pub trait WithNodeKind {
    fn kind(&self) -> NodeKind;
}

// TODO: impl WithNodeKind for Node

macro_rules! impl_with_node_kind {
    ($($ty: ty: $kind: expr),*) => {
        $(
            impl WithNodeKind for $ty {
                fn kind(&self) -> NodeKind {
                    $kind
                }
            }
        )*
    };
}

impl_with_node_kind!(
    ExprNode: NodeKind::Expr,
    StmtNode: NodeKind::Stmt,
    PatNode: NodeKind::Pat,
    BlockNode: NodeKind::Block,
    TyNode: NodeKind::Ty,
    PathNode<ExprRes>: NodeKind::ExprPath,
    PathNode<TyRes>: NodeKind::TyPath,
    VariantNode: NodeKind::Variant,
    ItemNode: NodeKind::Item,
    Mod: NodeKind::Root,
    ErrorNode: NodeKind::Error
);

impl Node {
    pub fn as_owner<'hir>(&'hir self) -> Option<OwnerNode<'hir>> {
        match self {
            Node::Item(item) => Some(OwnerNode::Item(item)),
            Node::Root(root) => Some(OwnerNode::Root(root)),
            _ => None,
        }
    }

    pub fn hir_id(&self) -> HirId {
        match self {
            Node::Expr(expr) => expr.id(),
            Node::Stmt(stmt) => stmt.id(),
            Node::Pat(pat) => pat.id(),
            Node::Block(block) => block.id(),
            Node::Ty(ty) => ty.id(),
            Node::ExprPath(path) => path.id(),
            Node::TyPath(path) => path.id(),
            Node::Variant(variant) => variant.id(),
            Node::Item(item) => HirId::new_owner(item.def_id()),
            Node::Root(_root) => HirId::new_owner(ROOT_DEF_ID),
            Node::Error(error) => error.id,
        }
    }

    pub fn kind(&self) -> NodeKind {
        match self {
            Node::Expr(_) => NodeKind::Expr,
            Node::Stmt(_) => NodeKind::Stmt,
            Node::Pat(_) => NodeKind::Pat,
            Node::Block(_) => NodeKind::Block,
            Node::Ty(_) => NodeKind::Ty,
            Node::ExprPath(_) => NodeKind::ExprPath,
            Node::TyPath(_) => NodeKind::TyPath,
            Node::Variant(_) => NodeKind::Variant,
            Node::Item(_) => NodeKind::Item,
            Node::Root(_) => NodeKind::Root,
            Node::Error(_) => NodeKind::Error,
        }
    }
}

pub enum OwnerNode<'hir> {
    Root(&'hir Mod),
    Item(&'hir ItemNode),
}

#[derive(Default, Debug)]
pub struct Owner {
    pub bodies: OwnerChildrenMap<Body>,

    // OwnerNode is the first child of nodes
    pub nodes: OwnerChildrenMap<Node>,
}

impl Owner {
    pub fn owner_node(&self) -> OwnerNode {
        self.nodes
            .get_expect(OWNER_SELF_CHILD_ID, "No owner self node in owner nodes")
            .as_owner()
            .expect("First node in owner nodes is non-owner node")
    }
}

#[derive(Debug)]
pub struct HIR {
    // TODO: Replace with IndexVec
    // Optimize: Late init IndexVec::with_capacity count of definitions in DefTable
    owners: DefMap<Owner>,
}

impl HIR {
    pub fn new() -> Self {
        Self {
            owners: Default::default(),
        }
    }

    pub fn add_owner(&mut self, def_id: DefId, owner: Owner) {
        self.owners.insert(def_id, owner);
    }

    // // Debug //
    // pub fn dump(&self) -> String {
    //     let dump_owner = |owner: &Owner| -> String {

    //     };

    //     self.owners
    //         .iter()
    //         .map(|(def_id, owner)| format!("{}: {}", def_id, dump_owner(owner)))
    //         .collect::<Vec<_>>()
    //         .join("\n")
    // }
}

#[derive(Clone, Copy)]
pub enum BodyOwnerKind {
    Func,
    Lambda,
    Value,
}

pub struct BodyOwner {
    pub def_id: DefId,
    pub kind: BodyOwnerKind,
}

impl BodyOwner {
    pub fn func(def_id: DefId) -> Self {
        Self {
            def_id,
            kind: BodyOwnerKind::Func,
        }
    }

    pub fn lambda(def_id: DefId) -> Self {
        Self {
            def_id,
            kind: BodyOwnerKind::Lambda,
        }
    }

    pub fn value(def_id: DefId) -> Self {
        Self {
            def_id,
            kind: BodyOwnerKind::Value,
        }
    }
}

#[derive(Debug)]
pub struct Body {
    pub params: Vec<Pat>,
    pub value: Expr,
}

impl Body {
    pub fn new(param: Vec<Pat>, value: Expr) -> Self {
        Self {
            params: param,
            value,
        }
    }

    pub fn id(&self) -> BodyId {
        BodyId(self.value.into())
    }
}

#[derive(Debug)]
pub struct PathSeg {
    ident: Ident,
    span: Span,
}

impl PathSeg {
    pub fn new(ident: Ident, span: Span) -> Self {
        Self { ident, span }
    }
}

impl Display for PathSeg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

/// Reduced version of `DefKind` for type definitions.
/// Handy in typeck.
/// Modules: No Root and Mod (same for `ExprDefKind`), because path referring
/// to mod is only possible in imports.
/// Locals: Locals are removed too, because there's a
/// specific kind of `Res::Local`.
/// DeclareBuiltin: It only exists for
/// one purpose -- simplification of lowering process, and it cannot appear as a
/// standalone path after lowering!
/// Variant: Removed because paths cannot refer to a specific variant but only
/// to constructors which are expressions. BUT! Path to variant resolution is
/// possible in patterns, also can come back to type paths for refinement types.
/// Error: Error resolution is removed too, but can be returned back if
/// resolution process will stably be recoverable.
/// Also check `ExprDefKind`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TyDefKind {
    TyAlias,
    Adt,
    TyParam,
}

impl Display for TyDefKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let def_kind: DefKind = (*self).into();
        write!(f, "{def_kind}")
    }
}

sub_enum_conversion! {
    TyDefKind <: DefKind {
        TyAlias,
        Adt,
        TyParam
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TyRes {
    Def(TyDefKind, DefId),
}

impl Display for TyRes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyRes::Def(kind, id) => write!(f, "{kind}{id}"),
        }
    }
}

/// Reduced version of `DefKind` for expression definitions.
/// Lambda: Remove, because lambdas are anonymous, hence we cannot refer to it
/// by path. Please, read `TyDefKind` description.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum ExprDefKind {
    Func,
    Value,
    Ctor,
    FieldAccessor,
    External,
}

impl Display for ExprDefKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let def_kind: DefKind = (*self).into();
        write!(f, "{def_kind}")
    }
}

sub_enum_conversion! {
    ExprDefKind <: DefKind {
        Func,
        Value,
        Ctor,
        FieldAccessor,
        External
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum ExprRes {
    Def(ExprDefKind, DefId),
    Local(HirId),
}

impl Display for ExprRes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprRes::Def(kind, id) => write!(f, "{kind}{id}"),
            ExprRes::Local(id) => write!(f, "{id}"),
        }
    }
}

#[derive(Debug)]
pub struct PathNode<Res> {
    id: HirId,
    res: Res,
    segments: Vec<PathSeg>,
    span: Span,
}

impl_with_span!(PathNode<Res>);

impl<Res> PathNode<Res> {
    pub fn new(id: HirId, res: Res, segments: Vec<PathSeg>, span: Span) -> Self {
        Self {
            id,
            res,
            segments,
            span,
        }
    }

    pub fn res(&self) -> &Res {
        &self.res
    }

    pub fn segments(&self) -> &[PathSeg] {
        self.segments.as_ref()
    }

    pub fn target_name(&self) -> Ident {
        self.segments.last().unwrap().ident
    }
}

impl_with_hir_id!(PathNode<Res>);

impl<Res> Display for PathNode<Res> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.segments()
                .iter()
                .map(|seg| format!("{seg}"))
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}
