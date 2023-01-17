/*!
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{NodeId, WithNodeId},
    cli::color::{Color, Colorize},
    dt::idx::declare_idx,
    resolve::{
        def::{DefId, DefMap, ROOT_DEF_ID},
        res::Res,
    },
    span::span::{Ident, Span},
};

use self::{
    expr::Expr,
    item::{Item, ItemId, Mod},
    pat::Pat,
    stmt::Stmt,
};

pub mod expr;
pub mod item;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod visitor;

type N<T> = Box<T>;

declare_idx!(OwnerId, DefId, "owner{}", Color::BrightCyan);
declare_idx!(BodyId, HirId, "body{}", Color::Green);
declare_idx!(OwnerChildId, u32, "owner_child#{}", Color::Cyan);

const FIRST_OWNER_CHILD_ID: OwnerChildId = OwnerChildId(0);

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
}

impl Display for HirId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.owner, self.id)
    }
}

// HIR Node is any Node in HIR with HirId
pub enum Node<'hir> {
    Expr(&'hir Expr),
    Stmt(&'hir Stmt),
    Item(&'hir Item),
    Pat(&'hir Pat),
}

impl<'hir> Node<'hir> {
    pub fn as_owner(&self) -> Option<OwnerNode<'hir>> {
        match self {
            Node::Item(item) => Some(OwnerNode::Item(item)),
            _ => None,
        }
    }
}

pub enum OwnerNode<'hir> {
    Root(&'hir Mod),
    Item(&'hir Item),
}

#[derive(Default)]
pub struct Owner<'hir> {
    pub bodies: OwnerChildrenMap<&'hir Body>,

    // OwnerNode is the first child of nodes
    pub nodes: OwnerChildrenMap<Node<'hir>>,
}

impl<'hir> Owner<'hir> {
    pub fn owner_node(&self) -> OwnerNode {
        self.nodes
            .get(&FIRST_OWNER_CHILD_ID)
            .unwrap()
            .as_owner()
            .unwrap()
    }
}

pub struct HIR<'hir> {
    owners: DefMap<Owner<'hir>>,
}

impl<'hir> HIR<'hir> {
    pub fn new() -> Self {
        Self {
            owners: Default::default(),
        }
    }

    pub fn add_owner(&mut self, def_id: DefId) {
        self.owners.insert(def_id, Owner::default());
    }

    pub fn expect_owner(&self, def_id: DefId) -> &mut Owner {
        self.owners.get_mut(&def_id).unwrap()
    }

    pub fn expect_owner_node(&self, def_id: DefId) -> OwnerNode {
        self.expect_owner(def_id).owner_node()
    }

    pub fn root(&self) -> &Mod {
        match self.expect_owner_node(&ROOT_DEF_ID) {
            OwnerNode::Root(root) => root,
            OwnerNode::Item(_) => unreachable!(),
        }
    }
}

pub struct Body {
    value: N<Expr>,
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

pub struct Path {
    id: HirId,
    res: Res,
    segments: Vec<PathSeg>,
    span: Span,
}

impl WithNodeId for Path {
    fn id(&self) -> NodeId {
        self.node_id
    }
}

impl Path {
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
}

impl Display for Path {
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
