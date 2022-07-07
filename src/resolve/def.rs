use std::{array, collections::HashMap, fmt::Display};

use crate::{
    ast::{item::ItemKind, NodeId, NodeMap, DUMMY_NODE_ID},
    cli::color::Colorize,
    span::span::{Ident, IdentKind, Kw, Span, Symbol},
};

#[derive(Clone, Copy)]
pub enum DefKind {
    Type,
    Mod,
    Decl,
}

impl DefKind {
    pub fn from_item_kind(kind: &ItemKind) -> DefKind {
        match kind {
            ItemKind::Type(_, _) => DefKind::Type,
            ItemKind::Mod(_, _) => DefKind::Mod,
            ItemKind::Decl(_, _, _) => DefKind::Decl,
        }
    }

    pub fn namespace(&self) -> Namespace {
        match self {
            DefKind::Type => Namespace::Type,
            DefKind::Mod => Namespace::Type,
            DefKind::Decl => Namespace::Value,
        }
    }
}

impl Display for DefKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DefKind::Type => "type alias",
                DefKind::Mod => "module",
                DefKind::Decl => "term declaration",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct DefId(pub u32);

impl DefId {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl Display for DefId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("#{}", self.as_usize()).magenta())
    }
}

pub const ROOT_DEF_ID: DefId = DefId(0);

pub type DefMap<T> = HashMap<DefId, T>;

/**
 * Definition of item, e.g. type
 */
pub struct Def {
    pub def_id: DefId,
    pub kind: DefKind,
    pub name: Ident,
}

impl Def {
    pub fn new(def_id: DefId, kind: DefKind, name: Ident) -> Self {
        Self { def_id, kind, name }
    }

    pub fn def_id(&self) -> DefId {
        self.def_id
    }

    pub fn kind(&self) -> &DefKind {
        &self.kind
    }

    pub fn name(&self) -> Ident {
        self.name
    }
}

impl Display for Def {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} `{}`{}", self.kind(), self.name(), self.def_id())
    }
}

#[derive(Clone, Copy)]
pub enum Namespace {
    Value, // Value namespace used for locals
    Type,  // Type namespace used for types and modules
}

impl Namespace {
    pub fn each(f: impl Fn(Namespace)) {
        f(Self::Value);
        f(Self::Type);
    }

    pub fn from_ident(ident: &Ident) -> Self {
        match ident.kind() {
            IdentKind::Var => Namespace::Value,
            IdentKind::Ty => Namespace::Type,
        }
    }
}

impl Display for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Namespace::Value => "value",
                Namespace::Type => "type",
            }
        )
    }
}

pub struct PerNS<T> {
    value: T,
    ty: T,
}

impl<T> PerNS<T> {
    pub fn get(&self, ns: Namespace) -> &T {
        match ns {
            Namespace::Value => &self.value,
            Namespace::Type => &self.ty,
        }
    }

    pub fn get_mut(&mut self, ns: Namespace) -> &mut T {
        match ns {
            Namespace::Value => &mut self.value,
            Namespace::Type => &mut self.ty,
        }
    }

    pub fn iter(&self) -> array::IntoIter<&T, 2> {
        [&self.value, &self.ty].into_iter()
    }
}

impl<T> Default for PerNS<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            value: Default::default(),
            ty: Default::default(),
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

impl Display for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "module{}",
            match self {
                ModuleId::Block(node_id) => format!("{}", node_id),
                ModuleId::Module(def_id) => format!("{}", def_id),
            }
        )
    }
}

type ModuleNamespace = HashMap<Symbol, DefId>;

/**
 * Module is a scope where items defined.
 */
pub struct Module {
    parent: Option<ModuleId>,
    kind: ModuleKind,
    per_ns: PerNS<ModuleNamespace>,
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

    pub fn namespaces(&self) -> &PerNS<ModuleNamespace> {
        &self.per_ns
    }

    /// Binds name to definition, returns old definitions if tried to redefine
    pub fn define(&mut self, ns: Namespace, sym: Symbol, def_id: DefId) -> Option<DefId> {
        self.per_ns.get_mut(ns).insert(sym, def_id)
    }

    pub fn get_by_ident(&self, ident: &Ident) -> Option<&DefId> {
        self.per_ns
            .get(Namespace::from_ident(ident))
            .get(&ident.sym())
    }
}

#[derive(Default)]
pub struct DefTable {
    modules: DefMap<Module>,
    blocks: NodeMap<Module>,
    node_id_def_id: HashMap<NodeId, DefId>,
    def_id_node_id: HashMap<DefId, NodeId>,

    /// Span of definition name
    def_id_span: HashMap<DefId, Span>,
    defs: Vec<Def>,
}

impl DefTable {
    pub(super) fn get_module_mut(&mut self, id: ModuleId) -> &mut Module {
        match id {
            ModuleId::Block(ref id) => self
                .blocks
                .get_mut(id)
                .expect(format!("No block found by {}", id).as_str()),
            ModuleId::Module(ref def_id) => self
                .modules
                .get_mut(def_id)
                .expect(format!("No module found by {}", def_id).as_str()),
        }
    }

    pub fn get_module(&self, module_id: ModuleId) -> &Module {
        match module_id {
            ModuleId::Block(ref id) => self
                .blocks
                .get(id)
                .expect(format!("No block found by {}", id).as_str()),
            ModuleId::Module(ref def_id) => self
                .modules
                .get(def_id)
                .expect(format!("No module found by {}", def_id).as_str()),
        }
    }

    pub(super) fn add_root_module(&mut self) -> ModuleId {
        assert!(self.defs.is_empty());
        // FIXME: Review usage of DUMMY_NODE_ID for root module
        self.define(
            DUMMY_NODE_ID,
            DefKind::Mod,
            &Ident::synthetic(Symbol::from_kw(Kw::Root)),
        );
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

    pub fn get_def_id(&self, node_id: NodeId) -> Option<DefId> {
        self.node_id_def_id.get(&node_id).copied()
    }

    pub fn get_node_id(&self, def_id: DefId) -> Option<NodeId> {
        self.def_id_node_id.get(&def_id).copied()
    }

    pub fn defs(&self) -> &[Def] {
        self.defs.as_ref()
    }
}
