use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{NodeId, NodeMap, WithNodeId},
    resolve::res::Res,
    span::span::{Ident, Span},
};

use self::{
    expr::Expr,
    item::{Item, ItemId},
};

/**
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
pub mod expr;
pub mod item;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod visitor;

type N<T> = Box<T>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub struct BodyId(NodeId);

impl BodyId {
    pub fn new(node_id: NodeId) -> Self {
        Self(node_id)
    }

    pub fn as_usize(&self) -> usize {
        self.0.as_usize()
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
        let id = ItemId::new(item.def_id());

        self.items.insert(item.def_id().as_usize(), item);

        id
    }

    pub fn add_body(&mut self, node_id: NodeId, body: Body) -> BodyId {
        let id = BodyId::new(node_id);
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
    res: Res,
    segments: Vec<PathSeg>,
    span: Span,
}

impl Path {
    pub fn new(res: Res, segments: Vec<PathSeg>, span: Span) -> Self {
        Self {
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
