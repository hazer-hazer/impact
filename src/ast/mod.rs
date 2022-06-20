use std::fmt::Display;

use crate::span::span::{Span, WithSpan};

use self::stmt::Stmt;

pub mod expr;
pub mod stmt;
pub mod ty;
pub mod visitor;

pub type N<T> = Box<T>;

pub type PR<T> = Result<T, ErrorNode>;

impl<T> Display for PR<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Ok(v) => v,
                Err(_) => "[ERROR]".to_string(),
            }
        )
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
