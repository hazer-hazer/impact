/*!
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
use core::panic;
use std::{collections::HashMap, fmt::Display};

use crate::{
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, Idx},
    resolve::{
        def::{DefId, DefMap, ROOT_DEF_ID},
        res::Res,
    },
    span::span::{Ident, Span, WithSpan},
};

use self::{
    expr::{BlockNode, Expr, ExprNode},
    item::{ItemId, ItemNode, Mod},
    pat::PatNode,
    stmt::StmtNode,
    ty::TyNode,
};

pub mod expr;
pub mod item;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod visitor;

type N<T> = Box<T>;

declare_idx!(OwnerId, DefId, "owner{}", Color::BrightCyan);
declare_idx!(wrapper BodyId, HirId, "body{}", Color::Green);
declare_idx!(OwnerChildId, u32, "owner_child#{}", Color::Cyan);

pub const OWNER_SELF_CHILD_ID: OwnerChildId = OwnerChildId(0);
pub const FIRST_OWNER_CHILD_ID: OwnerChildId = OwnerChildId(1);

type OwnerChildrenMap<T> = HashMap<OwnerChildId, T>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct HirId {
    owner: OwnerId,
    id: OwnerChildId,
}

impl HirId {
    pub fn new(owner: OwnerId, id: OwnerChildId) -> Self {
        Self { owner, id }
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
        write!(f, "{}:{}", self.owner, self.id)
    }
}

// HIR Node is any Node in HIR with HirId
macro_rules! hir_nodes {
    // identified by HirId --- other
    ($($name: ident $ty: tt,)* / $($other_name: ident $other_ty: tt,)*) => {
        pub enum Node {
            $(
                $ty($ty),
            )*
            $(
                $other_ty($other_ty),
            )*
        }

        impl Node {
            $(
                pub fn $name(&self) -> &$ty {
                    match self {
                        Self::$ty(inner) => inner,
                        _ => panic!(),
                    }
                }
            )*

            $(
                pub fn $other_name(&self) -> &$other_ty {
                    match self {
                        Self::$other_ty(inner) => inner,
                        _ => panic!(),
                    }
                }
            )*
        }

        impl<'hir> HIR {
            $(
                pub fn $name(&self, id: HirId) -> &$ty {
                    match self.node(id) {
                        Node::$ty(node) => node,
                        _ => panic!(),
                    }
                }
            )*
        }
    };
}

hir_nodes!(
    expr ExprNode,
    stmt StmtNode,
    pat PatNode,
    block BlockNode,
    ty TyNode,
    path PathNode,
    /
    item ItemNode,
    root Mod,
);

impl Node {
    pub fn as_owner<'hir>(&'hir self) -> Option<OwnerNode<'hir>> {
        match self {
            Node::ItemNode(item) => Some(OwnerNode::Item(item)),
            _ => None,
        }
    }

    pub fn hir_id(&self) -> HirId {
        match self {
            Node::ExprNode(expr) => expr.id,
            Node::StmtNode(stmt) => stmt.id,
            Node::PatNode(pat) => pat.id,
            Node::BlockNode(block) => block.id,
            Node::TyNode(ty) => ty.id,
            Node::PathNode(path) => path.id,
            Node::ItemNode(item) => HirId::new_owner(item.def_id()),
            Node::Mod(_root) => HirId::new_owner(ROOT_DEF_ID),
        }
    }
}

pub enum OwnerNode<'hir> {
    Root(&'hir Mod),
    Item(&'hir ItemNode),
}

#[derive(Default)]
pub struct Owner {
    pub bodies: OwnerChildrenMap<Body>,

    // OwnerNode is the first child of nodes
    pub nodes: OwnerChildrenMap<Node>,
}

impl Owner {
    pub fn owner_node(&self) -> OwnerNode {
        self.nodes
            .get(&FIRST_OWNER_CHILD_ID)
            .unwrap()
            .as_owner()
            .unwrap()
    }
}

pub struct HIR {
    // TODO: Replace with IndexVec
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

    // Getters //
    pub fn expect_owner(&self, def_id: DefId) -> &Owner {
        self.owners.get(&def_id).unwrap()
    }

    pub fn expect_owner_node(&self, def_id: DefId) -> OwnerNode {
        self.expect_owner(def_id).owner_node()
    }

    pub fn root(&self) -> &Mod {
        match self.expect_owner_node(ROOT_DEF_ID) {
            OwnerNode::Root(root) => root,
            OwnerNode::Item(_) => unreachable!(),
        }
    }

    pub fn item(&self, item_id: ItemId) -> &ItemNode {
        match self
            .owners
            .get(&item_id.inner().inner())
            .unwrap()
            .owner_node()
        {
            OwnerNode::Item(item) => item,
            OwnerNode::Root(_) => todo!(),
        }
    }

    pub fn node(&self, id: HirId) -> &Node {
        self.owners
            .get(&id.owner.inner())
            .unwrap()
            .nodes
            .get(&id.id)
            .unwrap()
    }
}

pub struct Body {
    value: Expr,
}

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

pub struct PathNode {
    pub id: HirId,
    res: Res,
    segments: Vec<PathSeg>,
    span: Span,
}

impl PathNode {
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

impl Display for PathNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.segments()
                .iter()
                .map(|seg| format!("{}", seg))
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

impl WithSpan for PathNode {
    fn span(&self) -> Span {
        self.span
    }
}

pub type Path = HirId;
