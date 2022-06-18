use crate::{
    parser::token::{Token, TokenKind},
    pp::PP,
    session::Session,
};
use std::fmt::{Display, Formatter};
use string_interner::{DefaultSymbol, StringInterner, Symbol as ISymbol};

macro_rules! symbols {
    ($($kw:ident: $str:expr),+) => {
        #[derive(Clone, Copy, Debug, PartialEq)]
        pub enum Kw {
            $($kw),+
        }

        impl Kw {
            pub fn as_str(&self) -> &str {
                match self {
                    $(Self::$kw => $str),+
                    _ => unreachable!(),
                }
            }

            pub fn from_str(string: &str) -> Option<Self> {
                match string {
                    $($str => Some(Self::$kw)),+
                    _ => None,
                }
            }

            pub fn as_usize(&self) -> usize {
                *self as usize
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

        pub struct Interner(StringInterner);

        impl Interner {
            pub fn new() -> Self {
                let interner = Self(StringInterner::default());
                $(interner.intern($kw);)+
                interner
            }

            pub fn intern(&mut self, string: &str) -> Symbol {
                Symbol::new(self.0.get_or_intern(string))
            }

            pub fn get(&self, sym: Symbol) -> &str {
                self.0
                    .resolve(sym.as_inner())
                    .expect(format!("Failed to resolve symbol {sym:?}").as_str())
            }

            pub fn as_kw(&self, sym: Symbol) -> Option<Kw> {
                Kw::from_str(self.get(sym))
            }
        }
    };
}

symbols! {
    Let: "let",
    In: "in",
    M: "m",
    Unknown: "[UNKNOWN]"
};

type SymbolInner = DefaultSymbol;

#[derive(Clone, Copy, PartialEq, Debug, Eq, Hash)]
pub struct Symbol(SymbolInner);

impl Symbol {
    pub fn new(sym: SymbolInner) -> Self {
        Self(sym)
    }

    pub fn kw(kw: Kw) -> Self {
        match kw {
            kw @ Kw::Let | kw @ Kw::In | kw @ Kw::M | kw @ Kw::Unknown => Symbol(
                SymbolInner::try_from_usize(kw as usize)
                    .expect(format!("Failed to construct symbol from keyword {}", kw).as_str()),
            ),
        }
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Ident {
    span: Span,
    sym: Symbol,
}

impl Ident {
    pub fn synthetic(sym: Symbol) -> Self {
        Self {
            span: Span::new_error(),
            sym,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> Symbol {
        self.sym
    }
}

impl WithSpan for Ident {
    fn span(&self) -> Span {
        self.span
    }
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
