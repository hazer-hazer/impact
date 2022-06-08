use crate::{pp::PP, session::Session};
use std::fmt::Formatter;
use string_interner::DefaultSymbol;

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
    val: T,
}

impl<'a, T> PP<'a> for Spanned<T> where T: PP<'a> {
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{}", self.val.ppfmt(sess))
    }
}

pub struct Ident {
    span: Span,
    sym: Symbol,
}

impl Ident {
    pub fn new(span: Span, sym: Symbol) -> Self {
        Self { span, sym }
    }
}

impl<'a> PP<'a> for Ident {
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{}", self.sym.ppfmt(sess))
    }
}
