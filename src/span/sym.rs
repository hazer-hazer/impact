use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::RwLock,
};

use once_cell::sync::Lazy;

use super::{impl_with_span, Span, WithSpan};
use crate::{dt::maps::enum_str_map, parser::lexer::LexerCharCheck};

static INTERNER: Lazy<RwLock<Interner>> = Lazy::new(|| Default::default());

enum_str_map! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub Kw {
        Let: "let" = 0,
        In: "in",
        Mod: "mod",
        Type: "type",
        Data: "data",
        Struct: "struct",
        Extern: "extern",
        Match: "match",

        Underscore: "_",
        Unit: "()", // `()`, allowed is type or expression

        // Reserved for name resolution //
        Root: "[ROOT]",

        Unknown: "[UNKNOWN]",
    }
}

type SymbolInner = u32;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(SymbolInner);

impl Symbol {
    // Note: Symbol must not have a public constructor,
    //  because we treat all constructed symbols as valid, i.e. interned.

    pub fn from_kw(kw: Kw) -> Symbol {
        kw.as_str().intern()
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

    pub fn original_string(&self) -> String {
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

    pub fn kind(&self) -> Option<IdentKind> {
        match self.as_str().chars().nth(0) {
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
        let str = self.as_str();
        !str.is_empty() && str.chars().all(|ch| ch.is_custom_op())
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
        self.sym().kind()
    }

    pub fn is_var(&self) -> bool {
        self.sym().is_var()
    }

    pub fn is_ty(&self) -> bool {
        self.sym().is_ty()
    }

    pub fn is_op(&self) -> bool {
        self.sym().is_op()
    }

    pub fn original_string(&self) -> String {
        self.sym().original_string()
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

impl_with_span!(Ident);
