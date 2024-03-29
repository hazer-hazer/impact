use once_cell::sync::Lazy;

use crate::{
    parser::token::{Token, TokenKind},
    session::{SourceId, DUMMY_SOURCE_ID},
};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::RwLock,
};

static INTERNER: Lazy<RwLock<Interner>> = Lazy::new(|| Default::default());

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Kw {
    Let = 0,
    In,
    Mod,
    Type,

    // Reserved for name resolution //
    Root,

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
            Kw::Mod => "mod",
            Kw::Type => "type",
            Kw::Root => "[root]",
            Kw::Unknown => "[UNKNOWN]",
        }
    }
}

impl Display for Kw {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

type SymbolInner = u32;

#[derive(Clone, Copy, PartialEq, Debug, Eq, Hash)]
pub struct Symbol(SymbolInner);

impl Symbol {
    // Note: Symbol must not have a public constructor,
    //  because we treat all constructed symbols as valid, i.e. interned.

    pub fn from_kw(kw: Kw) -> Symbol {
        Self::try_from(kw).expect("Failed to make a symbol from keyword")
    }

    pub fn intern(string: &str) -> Symbol {
        INTERNER.write().unwrap().intern(string)
    }

    pub fn as_str(&self) -> &str {
        INTERNER.read().unwrap().resolve(self)
    }

    pub fn as_inner(&self) -> SymbolInner {
        self.0
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl TryInto<Kw> for Symbol {
    type Error = ();

    fn try_into(self) -> Result<Kw, Self::Error> {
        match self.as_str() {
            "let" => Ok(Kw::Let),
            "in" => Ok(Kw::In),
            "m" => Ok(Kw::M),
            _ => Err(()),
        }
    }
}

impl TryFrom<Kw> for Symbol {
    type Error = ();

    fn try_from(kw: Kw) -> Result<Self, Self::Error> {
        Ok(Self::intern(kw.as_str()))
    }
}

#[derive(Default)]
struct Interner {
    symbols: HashMap<&'static str, Symbol>,
    strings: Vec<&'static str>,
}

impl Interner {
    fn intern<S: AsRef<str>>(&mut self, string: S) -> Symbol {
        let string = string.as_ref();

        if let Some(sym) = self.symbols.get(string) {
            return *sym;
        }

        // !Leaked
        let string = Box::leak(string.to_owned().into_boxed_str());
        let sym = Symbol(self.strings.len() as u32);

        self.symbols.insert(string, sym);
        self.strings.push(string);

        sym
    }

    fn resolve(&self, sym: &Symbol) -> &'static str {
        self.strings
            .get(sym.as_inner() as usize)
            .expect(format!("Failed to resolve symbol {sym:?}").as_str())
    }
}

pub type SpanPos = u32;
pub type SpanLen = u32;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Span {
    pos: SpanPos,
    len: SpanLen,
    source: SourceId,
}

impl Span {
    pub fn new_error() -> Self {
        Self {
            pos: SpanPos::MAX,
            len: SpanLen::MAX,
            source: DUMMY_SOURCE_ID,
        }
    }

    pub fn is_error(&self) -> bool {
        self.pos == SpanPos::MAX && self.len == SpanPos::MAX
    }

    pub fn new(pos: SpanPos, len: SpanLen, source: SourceId) -> Self {
        Self { pos, len, source }
    }

    pub fn lo(&self) -> SpanPos {
        self.pos
    }

    pub fn hi(&self) -> SpanPos {
        self.pos + self.len
    }

    pub fn len(&self) -> SpanLen {
        self.len
    }

    pub fn source(&self) -> SourceId {
        self.source
    }

    pub fn to(&self, end: Span) -> Self {
        assert!(self.source() == end.source());
        Span::new(
            std::cmp::min(self.pos, end.pos),
            std::cmp::max(self.hi(), end.hi()),
            self.source(),
        )
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.lo(), self.hi())
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

impl<T> Display for Spanned<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node())
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IdentKind {
    Var, // Lower-case identifiers are used for variables
    Ty,  // Capitalized identifiers are used for types and modules
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

    pub fn synthetic(sym: Symbol) -> Self {
        Self {
            span: Span::new_error(),
            sym,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn sym(&self) -> Symbol {
        self.sym
    }

    pub fn kind(&self) -> IdentKind {
        match self.sym().to_string().chars().nth(0) {
            Some(first) if first.is_uppercase() => IdentKind::Ty,
            Some(first) if first.is_lowercase() => IdentKind::Var,
            _ => panic!(),
        }
    }

    pub fn is_var(&self) -> bool {
        self.kind() == IdentKind::Var
    }

    pub fn is_ty(&self) -> bool {
        self.kind() == IdentKind::Ty
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.sym())
    }
}

impl WithSpan for Ident {
    fn span(&self) -> Span {
        self.span
    }
}
