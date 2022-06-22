pub enum DefKind {}

/**
 * Definition of item, e.g. type
 */
pub struct Def {
    kind: DefKind,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct DefId(u32);

pub enum Namespace {
    Value, // Value namespace used for locals
    Type,  // Type namespace used for types and modules
}
