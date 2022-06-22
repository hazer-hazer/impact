use super::def::DefId;

pub enum ResKind {
    Def(DefId),   // Top-level definition, e.g. imported function
    Local, // Local variable
    Error,
}

/**
 * The unit of name resolution.
 * Created for each name in source code after items are defined.
 */
pub struct Res {
    kind: ResKind,

}
