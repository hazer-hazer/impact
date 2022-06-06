use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Clone, Copy, PartialEq)]
pub struct Symbol(u32);

impl Symbol {
    pub fn new(val: u32) -> Self {
        Self(val)
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

#[derive(Default)]
pub struct Interner<'a> {
    strings: Vec<&'a str>,
    symbols: HashMap<&'a str, Symbol>,
}

impl<'a> Interner<'a> {
    // Don't even try to print symbols inside these methods or you're dead
    pub fn intern(&mut self, string: &'a str) -> Symbol {
        if let Some(&sym) = self.symbols.get(string) {
            return sym;
        }

        let sym = Symbol(self.strings.len() as u32);
        self.strings.push(string);
        self.symbols.insert(string, sym);
        sym
    }

    pub fn get_str(&self, sym: Symbol) -> &str {
        self.strings.get(sym.0 as usize).expect(
            format!(
                "Failed to find Symbol by id {}, symbol table is {:?}",
                sym.0, self.strings
            )
            .as_str(),
        )
    }
}
