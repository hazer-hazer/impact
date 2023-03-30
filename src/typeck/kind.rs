pub enum Kind {
    Type,
    // It's possible to avoid boxing just ascribing arity only,
    // but doing so we cannot distinguish `(* -> *) -> *` from `* -> * -> *`,
    // it is always the second variant.
    Func(Box<Kind>, Box<Kind>),
}
