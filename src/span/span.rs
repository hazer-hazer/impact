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

pub type SpanLen = u32;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub pos: u64,
    pub len: u32,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.pos, self.len)
    }
}
