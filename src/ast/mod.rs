use std::fmt::Display;

use crate::span::span::{Span, WithSpan};

use self::stmt::Stmt;

pub mod expr;
pub mod stmt;
pub mod ty;
pub mod visitor;

pub type N<T> = Box<T>;

pub type PR<T> = Result<T, ErrorNode>;

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

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub struct NodeId(u32);

#[derive(Default)]
pub struct AST {
    stmts: Vec<PR<N<Stmt>>>,
}

impl AST {
    pub fn new(stmts: Vec<PR<N<Stmt>>>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &Vec<PR<N<Stmt>>> {
        &self.stmts
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
        write!(f, "{}", self.span)
    }
}
