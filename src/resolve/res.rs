use std::{collections::HashMap, fmt::Display};

use crate::ast::NodeId;

use super::{builtin::Builtin, def::DefId};

// #[derive(Debug, Clone, Copy)]
// pub struct LocalId(NodeId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ResKind<LocalId> {
    Local(LocalId),
    Def(DefId), // Definition, e.g. imported function
    MakeBuiltin,
    Builtin(Builtin),
    Error,
}

/**
 * The unit of name resolution.
 * Created for each name in source code after items are defined.
 */
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Res<LocalId>
where
    LocalId: Copy,
{
    kind: ResKind<LocalId>,
}

impl<LocalId> Res<LocalId>
where
    LocalId: Copy,
{
    pub fn def(def_id: DefId) -> Self {
        Self {
            kind: ResKind::Def(def_id),
        }
    }

    pub fn local(id: LocalId) -> Self {
        Self {
            kind: ResKind::Local(id),
        }
    }

    pub fn declare_builtin() -> Self {
        Self {
            kind: ResKind::MakeBuiltin,
        }
    }

    pub fn builtin(builtin: Builtin) -> Self {
        Self {
            kind: ResKind::Builtin(builtin),
        }
    }

    pub fn error() -> Self {
        Self {
            kind: ResKind::Error,
        }
    }

    pub fn expect_def(&self) -> DefId {
        match self.kind {
            ResKind::Def(def_id) => def_id,
            _ => panic!("Res::expect_def"),
        }
    }

    pub fn kind(&self) -> &ResKind<LocalId> {
        &self.kind
    }
}

impl<LocalId> Display for Res<LocalId>
where
    LocalId: Display + Copy,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ResKind::Def(def_id) => write!(f, "{}", def_id),
            ResKind::Local(node_id) => write!(f, "{}", node_id),
            ResKind::MakeBuiltin => write!(f, "[`builtin`]"),
            ResKind::Builtin(bt) => write!(f, "[builtin {}]", bt),
            ResKind::Error => write!(f, "[ERROR]"),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct NamePath {
    node_id: NodeId,
}

impl NamePath {
    pub fn new(node_id: NodeId) -> Self {
        Self { node_id }
    }
}

#[derive(Default)]
pub struct Resolutions {
    resolutions: HashMap<NamePath, Res<NodeId>>,
}

impl Resolutions {
    pub fn set(&mut self, path: NamePath, res: Res<NodeId>) {
        assert!(self.resolutions.insert(path, res).is_none());
    }

    pub fn get(&self, path: NamePath) -> Option<Res<NodeId>> {
        self.resolutions.get(&path).copied()
    }

    pub fn get_resolutions(&self) -> &HashMap<NamePath, Res<NodeId>> {
        &self.resolutions
    }
}
