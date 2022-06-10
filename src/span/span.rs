use crate::{
    parser::token::{Token, TokenKind},
    pp::PP,
    session::Session,
};
use std::fmt::{Display, Formatter};
use string_interner::DefaultSymbol;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Kw {
    Let,
}

impl Display for Kw {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Kw::Let => "let",
            }
        )
    }
}

impl<'a> PP<'a> for Kw {
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{}", self)
    }
}

type SymbolInner = DefaultSymbol;

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Symbol(SymbolInner);

impl Symbol {
    pub fn new(sym: SymbolInner) -> Self {
        Self(sym)
    }

    pub fn as_inner(self) -> SymbolInner {
        self.0
    }

    pub fn as_kw(self, sess: &Session) -> Option<Kw> {
        sess.as_kw(self)
    }
}

impl<'a> PP<'a> for Symbol {
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{}", sess.get_str(*self))
    }
}

pub type SpanPos = u32;
pub type SpanLen = u32;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub pos: SpanPos,
    pub len: SpanLen,
}

impl Span {
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
        write!(f, "{}:{}", self.pos, self.len)
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
where T: WithSpan {
    fn span(&self) -> Span {
        self.as_ref().span()
    }
}

#[derive(Clone, Copy)]
pub struct Ident {
    span: Span,
    sym: Symbol,
}

impl Ident {
    pub fn new(span: Span, sym: Symbol) -> Self {
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

impl<'a> PP<'a> for Ident {
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{}", self.sym.ppfmt(sess))
    }
}
