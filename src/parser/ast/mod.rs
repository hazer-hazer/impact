use crate::{pp::PP, span::span::{Span, WithSpan}, span::span::Spanned};

use self::stmt::Stmt;

pub mod expr;
pub mod stmt;

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
                Ok(v) => v.ppfmt(sess).as_str(),
                Err(_) => "[ERROR]",
            }
        )
    }
}

impl<T> WithSpan for PR<T>
where T: WithSpan
{
    fn span(&self) -> Span {
        match self {
            Ok(ok) => ok.span(),
            Err(err) => err.span,
        }
    }
}

pub struct AST {
    stmts: Vec<Stmt>,
}

impl AST {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
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
