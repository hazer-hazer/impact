/*!
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{NodeId, WithNodeId},
    cli::color::{Color, Colorize},
    dt::idx::declare_idx,
    resolve::{def::DefId, res::Res},
    span::span::{Ident, Span},
};

use self::{
    expr::Expr,
    item::{Item, ItemId},
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


enum Node {

}

struct Owner<'hir> {
    bodies: OwnerChildrenMap<&'hir Body>,

}

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

pub struct Body {
    value: N<Expr>,
}

pub struct Root {
    items: Vec<ItemId>,
}

impl Root {
    pub fn items(&self) -> &[ItemId] {
        self.items.as_ref()
    }
}

pub struct HIR {
    root: Root,
    items: Vec<Item>,
    bodies: HashMap<BodyId, Body>,
}

impl HIR {
    pub fn new(defs_count: usize) -> Self {
        Self {
            root: Root {
                items: Default::default(),
            },
            // Note: As DefId is an incremental identifier and we use all DefId's we defined,
            //  we can set size of items list to what size we already know.
            items: Vec::with_capacity(defs_count),
            bodies: HashMap::default(),
        }
    }

    // Construction API //
    pub fn set_root(&mut self, items: Vec<ItemId>) {
        assert!(self.root.items.is_empty());
        self.root.items = items;
    }

    pub fn add_item(&mut self, item: Item) -> ItemId {
        let id = ItemId::new(item.owner_id());

        self.items.insert(item.def_id().as_usize(), item);

        id
    }

    pub fn add_body(&mut self, id: BodyId, body: Body) -> BodyId {
        self.bodies.insert(id, body);
        id
    }

    // Getters //
    pub fn item(&self, id: ItemId) -> &Item {
        self.items.get(id.as_usize()).unwrap()
    }

    pub fn body(&self, id: BodyId) -> &Body {
        self.bodies.get(&id).unwrap()
    }

    pub fn root(&self) -> &Root {
        &self.root
    }
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
