use std::fmt::Display;

use crate::{
    pp::PP,
    span::span::{Span, WithSpan},
};

use self::stmt::Stmt;

pub mod expr;
pub mod stmt;
pub mod visitor;

pub type N<T> = Box<T>;

pub type PR<T> = Result<T, ErrorNode>;

impl<'a, T> PP<'a> for PR<T>
where
    T: PP<'a>,
{
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        format!(
            "{}",
            match self {
                Ok(v) => v.ppfmt(sess),
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
