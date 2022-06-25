use std::collections::HashMap;

use crate::{
    ast::{item::ItemKind, NodeId, NodeMap},
    span::span::{Ident, Symbol},
};

pub enum DefKind {
    Type,
    Mod,
}

impl DefKind {
    pub fn from_item_kind(kind: &ItemKind) -> DefKind {
        match kind {
            ItemKind::Type(_, _) => DefKind::Type,
            ItemKind::Mod(_, _) => DefKind::Mod,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct DefId(pub u32);

impl DefId {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

pub const ROOT_DEF_ID: DefId = DefId(0);

pub type DefMap<T> = HashMap<DefId, T>;

/**
 * Definition of item, e.g. type
 */
pub struct Def {
    def_id: DefId,
    kind: DefKind,
    ident: Ident,
}

impl Def {
    pub fn new(def_id: DefId, kind: DefKind, ident: Ident) -> Self {
        Self {
            def_id,
            kind,
            ident,
        }
    }

    pub fn def_id(&self) -> DefId {
        self.def_id
    }

    pub fn kind(&self) -> &DefKind {
        &self.kind
    }
}

pub enum Namespace {
    Value, // Value namespace used for locals
    Type,  // Type namespace used for types and modules
}

pub struct PerNS<T> {
    value: T,
    ty: T,
}

impl<T> Default for PerNS<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            ..Default::default()
        }
    }
}

pub enum ModuleKind {
    Root,
    Block(NodeId),
    Def(DefId),
}

#[derive(Debug, Clone, Copy)]
pub enum ModuleId {
    Block(NodeId),
    Module(DefId),
}

/**
 * Module is a scope where items defined.
 */
pub struct Module {
    parent: Option<ModuleId>,
    kind: ModuleKind,
    per_ns: PerNS<HashMap<Symbol, DefId>>,
}

impl Module {
    pub fn root() -> Self {
        Self {
            parent: None,
            kind: ModuleKind::Root,
            per_ns: Default::default(),
        }
    }

    pub fn new(parent: ModuleId, kind: ModuleKind) -> Self {
        Self {
            parent: Some(parent),
            kind,
            per_ns: Default::default(),
        }
    }

    pub fn parent(&self) -> Option<ModuleId> {
        self.parent
    }

    pub fn kind(&self) -> &ModuleKind {
        &self.kind
    }
}

#[derive(Default)]
pub struct DefTable {
    modules: DefMap<Module>,
    blocks: NodeMap<Module>,
    node_id_def_id: HashMap<NodeId, DefId>,
    def_id_node_id: HashMap<DefId, NodeId>,
    defs: Vec<Def>,
}

impl DefTable {
    pub(super) fn get_module_mut(&mut self, id: ModuleId) -> &mut Module {
        match id {
            ModuleId::Block(ref id) => self
                .blocks
                .get_mut(id)
                .expect(format!("No block found by {:?}", id).as_str()),
            ModuleId::Module(ref def_id) => self
                .modules
                .get_mut(def_id)
                .expect(format!("No module found by {:?}", def_id).as_str()),
        }
    }

    pub fn get_module(&self, id: ModuleId) -> &Module {
        match id {
            ModuleId::Block(ref id) => self
                .blocks
                .get(id)
                .expect(format!("No block found by {:?}", id).as_str()),
            ModuleId::Module(ref def_id) => self
                .modules
                .get(def_id)
                .expect(format!("No module found by {:?}", def_id).as_str()),
        }
    }

    pub(super) fn add_root_module(&mut self) -> ModuleId {
        assert!(self.modules.insert(ROOT_DEF_ID, Module::root()).is_none());
        ModuleId::Module(ROOT_DEF_ID)
    }

    pub(super) fn add_module(&mut self, def_id: DefId, parent: ModuleId) -> ModuleId {
        assert!(self
            .modules
            .insert(def_id, Module::new(parent, ModuleKind::Def(def_id)))
            .is_none());
        ModuleId::Module(def_id)
    }

    pub(super) fn add_block(&mut self, node_id: NodeId, parent: ModuleId) -> ModuleId {
        assert!(self
            .blocks
            .insert(node_id, Module::new(parent, ModuleKind::Block(node_id)))
            .is_none());
        ModuleId::Block(node_id)
    }

    // TODO: Add accessibility modifiers?
    pub(super) fn define(&mut self, node_id: NodeId, kind: DefKind, ident: &Ident) -> DefId {
        let def_id = DefId(self.defs.len() as u32);
        self.defs.push(Def::new(def_id, kind, *ident));

        self.node_id_def_id.insert(node_id, def_id);
        self.def_id_node_id.insert(def_id, node_id);

        def_id
    }

    pub fn get_def(&self, def_id: DefId) -> Option<&Def> {
        self.defs.get(def_id.as_usize())
    }

    pub fn get_def_id(&self, node_id: &NodeId) -> Option<&DefId> {
        self.node_id_def_id.get(node_id)
    }

    pub fn get_node_id(&self, def_id: &DefId) -> Option<&NodeId> {
        self.def_id_node_id.get(def_id)
    }
}
