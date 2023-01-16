/*!
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{NodeId, WithNodeId},
    cli::color::{Color, Colorize},
    dt::idx::declare_idx,
    resolve::{
        def::{DefId, DefMap},
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
enum Node<'hir> {
    Expr(&'hir Expr),
    Stmt(&'hir Stmt),
    Item(&'hir Item),
    Pat(&'hir Pat),
}

enum OwnerNode<'hir> {
    Root(&'hir Mod),
    Item(&'hir Item),
}

struct Owner<'hir> {
    bodies: OwnerChildrenMap<&'hir Body>,
    nodes: OwnerChildrenMap<Node<'hir>>,
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

    pub fn add_owner(&mut self, def_id: DefId, owner: Owner) {
        self.owners.insert(def_id, owner);
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
    node_id: NodeId,
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
    pub fn new(node_id: NodeId, res: Res, segments: Vec<PathSeg>, span: Span) -> Self {
        Self {
            node_id,
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
