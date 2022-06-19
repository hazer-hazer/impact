use crate::{
    parser::token::{Token, TokenKind},
    pp::PP,
    session::Session,
};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Kw {
    Let = 0,
    In,

    // Reserved for typeck //
    M,

    Unknown,
}

impl Kw {
    pub fn as_str(&self) -> &str {
        match self {
            Kw::Let => "let",
            Kw::In => "in",
            Kw::M => "m",
            Kw::Unknown => "[UNKNOWN]",
        }
    }

    pub fn try_from_usize(disc: usize) -> Option<Self> {
        match disc {
            x if Kw::Let as usize == x => Some(Kw::Let),
            x if Kw::In as usize == x => Some(Kw::In),
            x if Kw::M as usize == x => Some(Kw::M),
            x if Kw::Unknown as usize == x => Some(Kw::Unknown),
            _ => None,
        }
    }
}

impl Display for Kw {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> PP<'a> for Kw {
    fn ppfmt(&self, _: &'a Session) -> String {
        format!("{}", self)
    }
}

pub type SpanPos = u32;
pub type SpanLen = u32;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Span {
    pub pos: SpanPos,
    pub len: SpanLen,
}

impl Span {
    pub fn new_error() -> Self {
        Self {
            pos: SpanPos::MAX,
            len: SpanLen::MAX,
        }
    }

    pub fn is_error(&self) -> bool {
        self.pos == SpanPos::MAX && self.len == SpanPos::MAX
    }

    pub fn new(pos: SpanPos, len: SpanLen) -> Self {
        Self { pos, len }
    }

    pub fn high(&self) -> SpanPos {
        self.pos + self.len
    }

    pub fn to(&self, end: Span) -> Self {
        Span::new(
            std::cmp::min(self.pos, end.pos),
            std::cmp::max(self.high(), end.high()),
        )
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.pos, self.pos + self.len)
    }
}

impl<'a> PP<'a> for Span {
    fn ppfmt(&self, _: &'a Session) -> String {
        format!("{} [len={}]", self.pos, self.len)
    }
}

pub struct Spanned<T> {
    span: Span,
    node: T,
}

impl<T> Clone for Spanned<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            span: self.span.clone(),
            node: self.node.clone(),
        }
    }
}

impl<T> Spanned<T> {
    pub fn new(span: Span, node: T) -> Self {
        Self { span, node }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn node(&self) -> &T {
        &self.node
    }
}

impl<'a, T> PP<'a> for Spanned<T>
where
    T: PP<'a>,
{
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{}", self.node.ppfmt(sess))
    }
}

impl<T> WithSpan for Spanned<T> {
    fn span(&self) -> Span {
        self.span
    }
}

pub trait WithSpan {
    fn span(&self) -> Span;
}

impl<T> WithSpan for Box<T>
where
    T: WithSpan,
{
    fn span(&self) -> Span {
        self.as_ref().span()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    span: Span,
    sym: String,
}

impl Ident {
    pub fn synthetic(sym: String) -> Self {
        Self {
            span: Span::new_error(),
            sym,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> String {
        self.sym
    }
}

impl WithSpan for Ident {
    fn span(&self) -> Span {
        self.span
    }
}

impl Ident {
    pub fn new(span: Span, sym: String) -> Self {
        Self { span, sym }
    }

    pub fn from_token(tok: Token) -> Self {
        match tok.kind {
            TokenKind::Ident(sym) => Ident {
                span: tok.span,
                sym,
            },
            _ => unreachable!(),
        }
    }
}
