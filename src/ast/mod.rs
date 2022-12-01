use std::{collections::HashMap, fmt::Display};

use crate::{
    cli::color::Colorize,
    span::span::{Ident, Span, WithSpan},
};

use self::item::Item;

pub mod expr;
pub mod item;
pub mod stmt;
pub mod ty;
pub mod validator;
pub mod visitor;

pub type N<T> = Box<T>;

pub type PR<T> = Result<T, ErrorNode>;

pub fn pr_display<T>(pr: &PR<T>) -> String
where
    T: Display,
{
    match pr {
        Ok(ok) => format!("{}", ok),
        Err(err) => format!("{}", err),
    }
}

pub fn pr_node_kind_str<T>(pr: &PR<T>) -> String
where
    T: NodeKindStr,
{
    match pr {
        Ok(ok) => ok.kind_str(),
        Err(_) => "I don't what is that...".to_string(),
    }
}

impl<T> WithSpan for PR<T>
where
    T: WithSpan,
{
    fn span(&self) -> Span {
        match self {
            Ok(ok) => ok.span(),
            Err(err) => err.span,
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Ord, Eq)]
pub struct NodeId(u32);

impl NodeId {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

pub const DUMMY_NODE_ID: NodeId = NodeId(u32::MAX);

pub trait WithNodeId {
    fn id(&self) -> NodeId;
}

impl Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("#{}", self.as_usize()).blue())
    }
}

pub type NodeMap<T> = HashMap<NodeId, T>;

#[derive(Default)]
pub struct AST {
    items: Vec<PR<N<Item>>>,
}

impl AST {
    pub fn new(items: Vec<PR<N<Item>>>) -> Self {
        Self { items }
    }

    pub fn items(&self) -> &[PR<N<Item>>] {
        self.items.as_ref()
    }
}

#[derive(Default)]
pub struct AstMetadata {
    last_node_index: u32,
}

impl AstMetadata {
    pub fn next_node_id(&mut self) -> NodeId {
        let id = NodeId(self.last_node_index);
        self.last_node_index += 1;
        id
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ErrorNode {
    span: Span,
}

impl ErrorNode {
    pub fn new(span: Span) -> Self {
        Self { span }
    }
}

impl Display for ErrorNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ERROR]")
    }
}

/// Give some pretty name for node-like structure in errors 😺
pub trait NodeKindStr {
    fn kind_str(&self) -> String;
}

impl<T> NodeKindStr for Box<T>
where
    T: NodeKindStr,
{
    fn kind_str(&self) -> String {
        (**self).kind_str()
    }
}

pub struct Path {
    id: NodeId,
    segments: Vec<Ident>,
}

impl Path {
    pub fn new(id: NodeId, segments: Vec<Ident>) -> Self {
        Self { id, segments }
    }

    pub fn segments(&self) -> &Vec<Ident> {
        &self.segments
    }

    pub fn prefix_str(&self, to: usize) -> String {
        let prefix = self.segments()[0..to]
            .iter()
            .map(|seg| format!("{}", seg))
            .collect::<Vec<_>>()
            .join(".");

        if prefix.is_empty() {
            "current scope".to_string()
        } else {
            prefix
        }
    }

    pub fn prefix_span(&self, to: usize) -> Span {
        self.segments()[0..=to]
            .iter()
            .map(|seg| seg.span())
            .reduce(|prefix, seg| prefix.to(seg))
            .unwrap()
    }
}

impl WithNodeId for Path {
    fn id(&self) -> NodeId {
        self.id
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
