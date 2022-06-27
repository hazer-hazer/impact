use std::{collections::HashMap, fmt::Display};

use crate::span::span::{Span, WithSpan};

use self::item::Item;

pub mod expr;
pub mod item;
pub mod stmt;
pub mod ty;
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
        Err(err) => "I don't what is that...".to_string(),
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

#[derive(Debug)]
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
