use std::{collections::HashMap, fmt::Display};

use super::def::{Def, DefId, DefKind};
use crate::{ast::NodeId, span::sym::Ident};

// TODO: For now, candidates are useless, because we just suggest anything with
// the same name, but for example, if we resolve module, we should suggest some
// modules, not locals or something from value namespace.
#[derive(Clone, Debug)]
pub enum Candidate {
    None,
    Local(Ident),
    Defs(Vec<Def>),
}

impl Candidate {
    pub fn defs(defs: Vec<Def>) -> Self {
        Self::Defs(defs)
    }

    pub fn set_if_none(&mut self, other: Candidate) {
        match self {
            Candidate::None => *self = other,
            Candidate::Local(_) | Candidate::Defs(_) => {},
        }
    }

    pub fn set_local(&mut self, local: Ident) {
        match self {
            Candidate::None => *self = Self::Local(local),
            Candidate::Local(_) | Candidate::Defs(_) => {},
        }
    }

    pub fn set_defs(&mut self, defs: Vec<Def>) {
        match self {
            Candidate::None => *self = Candidate::Defs(defs),
            Candidate::Local(_) | Candidate::Defs(_) => {},
        }
    }
}

impl Display for Candidate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Candidate::None => write!(f, "NONE"),
            Candidate::Local(local) => write!(f, "local{local}"),
            Candidate::Defs(defs) => write!(
                f,
                "defs[{}]",
                defs.iter()
                    .map(|&def| format!("{def}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ResKind {
    Local(NodeId),
    Def(DefId),
    DeclareBuiltin,
    Err,
}

/// The unit of name resolution.
/// Created for each name in source code after items are defined.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Res {
    kind: ResKind,
}

impl Res {
    pub fn def(def_id: DefId) -> Self {
        Self {
            kind: ResKind::Def(def_id),
        }
    }

    pub fn local(id: NodeId) -> Self {
        Self {
            kind: ResKind::Local(id),
        }
    }

    pub fn declare_builtin() -> Self {
        Self {
            kind: ResKind::DeclareBuiltin,
        }
    }

    pub fn error() -> Self {
        Self { kind: ResKind::Err }
    }

    pub fn expect_def(&self) -> DefId {
        match self.kind {
            ResKind::Def(def_id) => def_id,
            _ => panic!("Res::expect_def"),
        }
    }

    pub fn kind(&self) -> &ResKind {
        &self.kind
    }
}

impl Display for Res
where
    NodeId: Display + Copy,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ResKind::Def(def_id) => write!(f, "{def_id}"),
            ResKind::Local(node_id) => write!(f, "{node_id}"),
            ResKind::DeclareBuiltin => write!(f, "[`builtin`]"),
            ResKind::Err => write!(f, "[ERROR]"),
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

#[derive(Default, Debug)]
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
