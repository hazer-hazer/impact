use std::fmt::Display;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Kind {
    Ty,
    // It's possible to avoid boxing just ascribing arity only,
    // but doing so we cannot distinguish `(* -> *) -> *` from `* -> * -> *`,
    // it is always the second variant.
    Cons(Box<Kind>, Box<Kind>),
}

impl Kind {
    pub fn ty() -> Self {
        Self::Ty
    }

    pub fn cons(domain: Kind, range: Kind) -> Self {
        Self::Cons(Box::new(domain), Box::new(range))
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Ty => write!(f, "*"),
            Kind::Cons(domain, range) => write!(f, "{} -> {}", domain, range),
        }
    }
}
