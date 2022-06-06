use crate::dt::sync::Lock;
use crate::session::session::with_session_globals;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, Copy, PartialEq)]
pub struct Symbol(u32);

impl Symbol {
    pub fn as_str(&self) -> &str {
        with_session_globals(|globals| unsafe {
            std::mem::transmute::<&str, &str>(globals.interner.get_str(*self))
        })
    }

    pub fn intern(string: &str) -> Self {
        with_session_globals(|globals| globals.interner.intern(string))
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
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

#[derive(Default, Debug)]
struct InternerInner {
    strings: Vec<&'static str>,
    symbols: HashMap<&'static str, Symbol>,
}

#[derive(Default, Debug)]
pub struct Interner(Lock<InternerInner>);

impl Interner {
    // Don't even try to print symbols inside these methods or you're dead
    pub fn intern(&self, string: &str) -> Symbol {
        let mut inner = self.0.lock();
        if let Some(&sym) = inner.symbols.get(string) {
            return sym;
        }

        let sym = Symbol(inner.strings.len() as u32);
        let string: &'static str = unsafe { &*(string as *const str) };
        inner.strings.push(string);
        inner.symbols.insert(string, sym);
        sym
    }

    pub fn get_str(&self, sym: Symbol) -> &str {
        let inner = self.0.lock();
        inner.strings.get(sym.0 as usize).expect(
            format!(
                "Failed to find Symbol by id {}, symbol table is {:?}",
                sym.0, inner.strings
            )
            .as_str(),
        )
    }
}
