use once_cell::sync::Lazy;

use crate::{
    cli::verbose,
    parser::{
        lexer::LexerCharCheck,
        token::{Token, TokenKind},
    },
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

    Underscore,
    Unit, // `()`, allowed is type or expression

    // Reserved for name resolution //
    Root,

    Unknown,
}

impl Kw {
    pub fn as_str(&self) -> &str {
        match self {
            Kw::Let => "let",
            Kw::In => "in",
            Kw::Mod => "mod",
            Kw::Type => "type",
            Kw::Underscore => "_",
            Kw::Unit => "()",
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(SymbolInner);

impl Symbol {
    // Note: Symbol must not have a public constructor,
    //  because we treat all constructed symbols as valid, i.e. interned.

    pub fn from_kw(kw: Kw) -> Symbol {
        Self::try_from(kw).expect("Failed to make a symbol from keyword")
    }

    pub fn intern(str: &str) -> Symbol {
        INTERNER.write().unwrap().intern(str)
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

    pub fn is_op(&self) -> bool {
        let str = self.as_str();
        !str.is_empty() && str.chars().all(|ch| ch.is_custom_op())
    }

    pub fn looks_like(&self) -> String {
        if self.is_op() {
            format!("({})", self)
        } else {
            self.to_string()
        }
    }

    /// Checks if symbol is a name but not an operator
    pub fn is_non_op(&self) -> bool {
        self.as_str()
            .chars()
            .all(|ch| ch == '_' || ch.is_alphanumeric())
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Symbol({};`{}`)", self.0, self.as_str())
    }
}

impl TryInto<Kw> for Symbol {
    type Error = ();

    fn try_into(self) -> Result<Kw, Self::Error> {
        match self.as_str() {
            "let" => Ok(Kw::Let),
            "in" => Ok(Kw::In),
            "mod" => Ok(Kw::Mod),
            "type" => Ok(Kw::Type),
            "[root]" => Ok(Kw::Root),
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
            .expect(format!("Failed to resolve symbol {}", sym.0).as_str())
    }
}

pub trait Internable {
    fn intern(&self) -> Symbol;
}

impl Internable for String {
    fn intern(&self) -> Symbol {
        Symbol::intern(self)
    }
}

impl<'a> Internable for &'a str {
    fn intern(&self) -> Symbol {
        Symbol::intern(self)
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
            pos: 0,
            len: 1,
            source: DUMMY_SOURCE_ID,
        }
    }

    pub fn is_error(&self) -> bool {
        self.source == DUMMY_SOURCE_ID
    }

    pub fn new(pos: SpanPos, len: SpanLen, source: SourceId) -> Self {
        Self { pos, len, source }
    }

    pub fn new_file_top(source: SourceId) -> Self {
        Self {
            pos: 0,
            len: 1,
            source,
        }
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
        let lo = std::cmp::min(self.pos, end.pos);
        let hi = std::cmp::max(self.hi(), end.hi());
        assert!(hi >= lo);
        Span::new(lo, hi - lo, self.source())
    }

    pub fn point_after_hi(&self) -> Self {
        Self {
            pos: self.hi(),
            len: 1,
            source: self.source(),
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.lo(), self.hi())
    }
}

#[derive(Debug, Copy)]
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
    Op,  // Operator identifier, e.g. `(+)` (without parentheses)
}

impl Ident {
    pub fn new(span: Span, sym: Symbol) -> Self {
        Self { span, sym }
    }

    pub fn from_token(tok: Token) -> Self {
        Ident {
            span: tok.span,
            sym: match tok.kind {
                TokenKind::Kw(Kw::Unit) => "()".intern(),
                TokenKind::OpIdent(sym) | TokenKind::Ident(sym) => sym,
                _ => unreachable!(),
            },
        }
    }

    pub fn synthetic(sym: Symbol) -> Self {
        Self {
            span: Span::new_error(),
            sym,
        }
    }

    pub fn kw(kw: Kw) -> Self {
        Self {
            span: Span::new_error(),
            sym: Symbol::from_kw(kw),
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn sym(&self) -> Symbol {
        self.sym
    }

    pub fn as_str(&self) -> &str {
        self.sym.as_str()
    }

    pub fn kind(&self) -> Option<IdentKind> {
        match self.sym().to_string().chars().nth(0) {
            Some(first) if first.is_uppercase() => Some(IdentKind::Ty),
            Some(first) if first.is_lowercase() => Some(IdentKind::Var),
            Some(first) if first.is_custom_op() => Some(IdentKind::Op),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        match self.kind() {
            Some(IdentKind::Var) => true,
            _ => false,
        }
    }

    pub fn is_ty(&self) -> bool {
        match self.kind() {
            Some(IdentKind::Ty) => true,
            _ => false,
        }
    }

    pub fn is_op(&self) -> bool {
        match self.kind() {
            Some(IdentKind::Op) => true,
            _ => false,
        }
    }

    pub fn original_string(&self) -> String {
        if self.is_op() {
            format!("({})", self)
        } else {
            self.to_string()
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // if self.is_op() {
        //     write!(f, "({})", self.sym())
        // } else {
        write!(f, "{}", self.sym())
        // }
    }
}

impl WithSpan for Ident {
    fn span(&self) -> Span {
        self.span
    }
}
