use std::{collections::HashMap, fmt::Display};

use self::item::Item;
use crate::{
    cli::color::{Color, ColorizedStruct},
    dt::idx::{declare_idx, IndexVec},
    parser::token::{Token, TokenKind},
    span::{
        impl_with_span,
        sym::{Ident, Symbol},
        Span, WithSpan,
    },
};

pub mod expr;
pub mod item;
pub mod map;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod validator;
pub mod visitor;

pub type N<T> = Box<T>;
pub type PR<T> = Result<T, ErrorNode>;

impl<T> WithNodeId for PR<T>
where
    T: WithNodeId,
{
    fn id(&self) -> NodeId {
        match self {
            Ok(node) => node.id(),
            Err(err) => err.id(),
        }
    }
}

pub fn pr_display<T>(pr: &PR<T>) -> String
where
    T: Display,
{
    match pr {
        Ok(ok) => format!("{ok}"),
        Err(err) => format!("{err}"),
    }
}

pub fn prs_display_join<T>(prs: &[PR<T>], sep: &str) -> String
where
    T: Display,
{
    prs.iter().map(pr_display).collect::<Vec<_>>().join(sep)
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

declare_idx!(NodeId, u32, "#{}", Color::Blue);

pub const ROOT_NODE_ID: NodeId = NodeId(0);
pub const DUMMY_NODE_ID: NodeId = NodeId(u32::MAX);

pub trait WithNodeId {
    fn id(&self) -> NodeId;
}

macro_rules! impl_with_node_id {
    ($ty: ident $(<$($generic: ident)+>)?) => {
        impl<$($($generic,)+)?> WithNodeId for $ty<$($($generic,)+)?> {
            fn id(&self) -> NodeId {
                self.id
            }
        }
    };
}

pub(crate) use impl_with_node_id;

pub type NodeMap<T> = IndexVec<NodeId, Option<T>>;

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

impl WithNodeId for AST {
    fn id(&self) -> NodeId {
        ROOT_NODE_ID
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReservedNodeId {
    DeclareBuiltin,
}

pub struct AstMetadata {
    last_node_index: NodeId,
    reserved: HashMap<ReservedNodeId, NodeId>,
}

impl AstMetadata {
    pub fn new() -> Self {
        Self {
            last_node_index: ROOT_NODE_ID.next(),
            reserved: Default::default(),
        }
    }

    pub fn next_node_id(&mut self) -> NodeId {
        let id = self.last_node_index;
        self.last_node_index.inc();
        id
    }

    pub fn reserve(&mut self, kind: ReservedNodeId) -> NodeId {
        let node_id = self.next_node_id();
        assert!(self.reserved.insert(kind, node_id).is_none());
        node_id
    }
}

#[derive(Debug, Clone)]
pub struct ErrorNode {
    span: Span,
    parsed: Option<String>,
}

impl ErrorNode {
    pub fn new(span: Span) -> Self {
        Self { span, parsed: None }
    }

    pub fn new_parsed<T>(node: T) -> Self
    where
        T: WithSpan + Display,
    {
        Self {
            span: node.span(),
            parsed: Some(format!("{node}")),
        }
    }

    pub fn parsed(&self) -> Option<&String> {
        self.parsed.as_ref()
    }
}

impl Display for ErrorNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ERROR]")
    }
}

impl WithNodeId for ErrorNode {
    fn id(&self) -> NodeId {
        return DUMMY_NODE_ID;
    }
}

#[derive(Debug, Clone)]
pub struct IdentNode {
    pub id: NodeId,
    pub ident: Ident,
}

impl_with_node_id!(IdentNode);

impl IdentNode {
    pub fn new(id: NodeId, ident: Ident) -> Self {
        Self { id, ident }
    }
}

impl Display for IdentNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ident.fmt(f)
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

#[derive(Debug, Clone)]
pub struct PathSeg {
    name: PR<Ident>,
    span: Span,
}

impl_with_span!(PathSeg);

impl PathSeg {
    pub fn new(name: PR<Ident>, span: Span) -> Self {
        Self { name, span }
    }

    pub fn expect_name(&self) -> &Ident {
        self.name
            .as_ref()
            .expect("Error PathSeg::name in `PathSeg::expect_name`")
    }

    pub fn is_from_ty_ns(&self) -> bool {
        self.name.as_ref().map_or(false, |name| name.is_ty())
    }
}

impl Display for PathSeg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pr_display(&self.name))
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    id: NodeId,
    segments: Vec<PathSeg>,
    span: Span,
}

impl_with_node_id!(Path);
impl_with_span!(Path);

impl Path {
    pub fn new(id: NodeId, segments: Vec<PathSeg>, span: Span) -> Self {
        Self { id, segments, span }
    }

    pub fn new_infix_op(id: NodeId, tok: Token) -> Self {
        let segments = vec![PathSeg::new(
            Ok(Ident::new(
                tok.span(),
                match tok.kind {
                    TokenKind::Op(op) => Symbol::intern(&op.to_string()),
                    TokenKind::CustomOp(op) => op,
                    _ => panic!(),
                },
            )),
            tok.span,
        )];
        Self {
            id,
            segments,
            span: tok.span,
        }
    }

    pub fn prefix_str(&self, to: usize) -> String {
        let prefix = self.segments()[0..to]
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(".");

        if prefix.is_empty() {
            "current scope".to_string()
        } else {
            prefix
        }
    }

    pub fn original_str(&self) -> String {
        self.segments()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(".")
    }

    pub fn prefix_span(&self, to: usize) -> Span {
        self.segments()[0..=to]
            .iter()
            .map(|seg| seg.span())
            .reduce(|prefix, seg| prefix.to(seg))
            .expect("Empty Path in `Path::prefix_span`")
    }

    pub fn target_name(&self) -> Ident {
        *self
            .segments()
            .last()
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .unwrap()
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
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

pub trait IsBlockEnded {
    fn is_block_ended(&self) -> bool;
}

macro_rules! is_block_ended {
    ($pr: expr) => {
        match &$pr {
            Err(_) => false,
            Ok(node) => node.is_block_ended(),
        }
    };
}

pub(crate) use is_block_ended;
