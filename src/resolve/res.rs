use std::{collections::HashMap, fmt::Display};

use crate::ast::NodeId;

use super::def::DefId;

// #[derive(Debug, Clone, Copy)]
// pub struct LocalId(NodeId);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResKind {
    Def(DefId),    // Top-level definition, e.g. imported function
    Local(NodeId), // Local variable
    Error,
}

/**
 * The unit of name resolution.
 * Created for each name in source code after items are defined.
 */
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Res {
    kind: ResKind,
}

impl Res {
    pub fn def(def_id: DefId) -> Self {
        Self {
            kind: ResKind::Def(def_id),
        }
    }

    pub fn local(node_id: NodeId) -> Self {
        Self {
            kind: ResKind::Local(node_id),
        }
    }

    pub fn error() -> Self {
        Self {
            kind: ResKind::Error,
        }
    }

    pub fn kind(&self) -> &ResKind {
        &self.kind
    }
}

impl Display for Res {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ResKind::Def(def_id) => write!(f, "{}", def_id),
            ResKind::Local(node_id) => write!(f, "{}", node_id),
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
    resolutions: HashMap<NamePath, Res>,
}

impl Resolutions {
    pub fn set(&mut self, path: NamePath, res: Res) {
        assert!(self.resolutions.insert(path, res).is_none());
    }

    pub fn get(&self, path: NamePath) -> Option<Res> {
        self.resolutions.get(&path).copied()
    }

    pub fn get_resolutions(&self) -> &HashMap<NamePath, Res> {
        &self.resolutions
    }
}
