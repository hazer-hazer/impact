use crate::ast::NodeId;

use super::def::DefId;

#[derive(Debug, Clone, Copy)]
pub struct LocalId(NodeId);

pub enum ResKind {
    Def(DefId),     // Top-level definition, e.g. imported function
    Local(LocalId), // Local variable
    Error,
}

/**
 * The unit of name resolution.
 * Created for each name in source code after items are defined.
 */
pub struct Res {
    kind: ResKind,
}
