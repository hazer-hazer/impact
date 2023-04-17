use std::{array, collections::HashMap, fmt::Display};

use super::builtin::{Builtin, DeclareBuiltin};
use crate::{
    ast::{item::ItemKind, NodeId, NodeMap, DUMMY_NODE_ID, ROOT_NODE_ID},
    cli::{
        color::{Color, ColorizedStruct},
        verbose,
    },
    dt::idx::{declare_idx, Idx, IndexVec},
    message::message::MessageBuilder,
    span::{
        source::SourceId,
        sym::{Ident, IdentKind, Internable, Kw, Symbol},
        Span,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefKind {
    Root,
    TyAlias,
    Mod,
    Func,
    Value,
    Lambda,
    Data,
    Variant,
    Ctor,
    FieldAccessor,
    TyParam,
    External,

    Local,

    DeclareBuiltin,
}

impl DefKind {
    pub fn from_item_kind(kind: &ItemKind) -> DefKind {
        match kind {
            ItemKind::Type(..) => DefKind::TyAlias,
            ItemKind::Mod(..) => DefKind::Mod,
            ItemKind::Decl(_, params, _) if params.is_empty() => DefKind::Value,
            ItemKind::Decl(..) => DefKind::Func,
            ItemKind::Adt(..) => DefKind::Data,
            ItemKind::Extern(_) => panic!(),
        }
    }

    pub fn namespace(&self) -> Namespace {
        match self {
            DefKind::TyAlias => Namespace::Type,
            DefKind::Mod => Namespace::Type,
            DefKind::Func => Namespace::Value,
            DefKind::Root => Namespace::Type,
            DefKind::Value => Namespace::Value,
            DefKind::DeclareBuiltin => Namespace::Value,
            DefKind::Lambda => Namespace::Value,
            DefKind::External => Namespace::Value,
            DefKind::Data => Namespace::Type,
            DefKind::Variant => Namespace::Type,
            DefKind::Ctor => Namespace::Value,
            DefKind::FieldAccessor => Namespace::Value,
            DefKind::TyParam => Namespace::Type,
            DefKind::Local => unreachable!(),
        }
    }
}

impl Display for DefKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DefKind::TyAlias => "type alias",
                DefKind::Mod => "module",
                DefKind::Func => "function",
                DefKind::Value => "value",
                DefKind::Lambda => "lambda",
                DefKind::External => "external item",
                DefKind::Root => "[ROOT]",
                DefKind::DeclareBuiltin => "[`builtin`]",
                DefKind::Local => "local",
                DefKind::Data => "data type",
                DefKind::Variant => "variant",
                DefKind::Ctor => "constructor",
                DefKind::FieldAccessor => "field accessor",
                DefKind::TyParam => "type parameter",
            }
        )
    }
}

declare_idx!(DefId, u32, "#{}", Color::Magenta);

pub const ROOT_DEF_ID: DefId = DefId(0);

pub type DefMap<T> = IndexVec<DefId, Option<T>>;

/// Definition of item, e.g. type
#[derive(Debug, Clone)]
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

#[derive(Clone, Copy, PartialEq, Debug)]
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
            Some(IdentKind::Op | IdentKind::Var) => Namespace::Value,
            Some(IdentKind::Ty) => Namespace::Type,
            None => panic!(),
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum ModuleKind {
    Root,
    Block(NodeId),
    Def(DefId),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ModuleId {
    Block(NodeId),
    Def(DefId),
}

impl ModuleId {
    pub fn as_module(&self) -> DefId {
        match self {
            ModuleId::Block(_) => panic!(),
            &ModuleId::Def(def_id) => def_id,
        }
    }

    pub fn as_block(&self) -> NodeId {
        match self {
            &ModuleId::Block(node_id) => node_id,
            ModuleId::Def(_) => panic!(),
        }
    }
}

pub const ROOT_MODULE_ID: ModuleId = ModuleId::Def(ROOT_DEF_ID);

impl Display for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "module{}",
            match self {
                ModuleId::Block(node_id) => format!("{}", node_id),
                ModuleId::Def(def_id) => format!("{}", def_id),
            }
        )
    }
}

type ModuleNamespace = HashMap<Symbol, DefId>;

/// Module is a scope where items defined.
#[derive(Debug)]
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

    // FIXME: Use `Symbol` instead of `Ident`
    pub fn get_from_ns(&self, ns: Namespace, ident: &Ident) -> Option<DefId> {
        self.per_ns.get(ns).get(&ident.sym()).copied()
    }

    #[deprecated = "Fails on `()` as it can be both in value and type namespace. Use `get_from_ns`"]
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
    node_id_def_id: NodeMap<DefId>,
    def_id_node_id: DefMap<NodeId>,

    declare_builtin: Option<DeclareBuiltin>,
    builtins: HashMap<Builtin, DefId>,
    def_id_builtins: DefMap<Builtin>,
    main_func: Option<DefId>,

    /// Span of definition name
    def_name_span: HashMap<DefId, Span>,

    defs: Vec<Def>,
}

impl DefTable {
    pub(super) fn get_module_mut(&mut self, id: ModuleId) -> &mut Module {
        match id {
            ModuleId::Block(id) => self
                .blocks
                .get_mut_expect(id, format!("No block found by {}", id).as_str()),
            ModuleId::Def(def_id) => self
                .modules
                .get_mut_expect(def_id, format!("No module found by {}", def_id).as_str()),
        }
    }

    pub fn get_module(&self, module_id: ModuleId) -> &Module {
        match module_id {
            ModuleId::Block(id) => self
                .blocks
                .get_expect(id, format!("No block found by {}", id).as_str()),
            ModuleId::Def(def_id) => self
                .modules
                .get_expect(def_id, format!("No module found by {}", def_id).as_str()),
        }
    }

    pub(super) fn add_root_module(&mut self, source: SourceId) -> ModuleId {
        assert!(self.defs.is_empty());
        // FIXME: Review usage of DUMMY_NODE_ID for root module
        let def_id = self.define(
            ROOT_NODE_ID,
            DefKind::Root,
            &Ident::new(Span::new_file_top(source), Symbol::from_kw(Kw::Root)),
        );
        assert!(self.modules.insert(ROOT_DEF_ID, Module::root()).is_none());
        assert_eq!(def_id, ROOT_DEF_ID);
        ModuleId::Def(def_id)
    }

    pub(super) fn add_module(&mut self, def_id: DefId, parent: ModuleId) -> ModuleId {
        assert!(self
            .modules
            .insert(def_id, Module::new(parent, ModuleKind::Def(def_id)))
            .is_none());
        ModuleId::Def(def_id)
    }

    pub(super) fn add_block(&mut self, node_id: NodeId, parent: ModuleId) -> ModuleId {
        assert!(self
            .blocks
            .insert(node_id, Module::new(parent, ModuleKind::Block(node_id)))
            .is_none());
        ModuleId::Block(node_id)
    }

    // TODO: Add accessibility modifiers?
    pub fn define(&mut self, node_id: NodeId, kind: DefKind, name: &Ident) -> DefId {
        let def_id = DefId(self.defs.len() as u32);
        self.defs.push(Def::new(def_id, kind, *name));

        verbose!("def {} span {}", def_id, name.span());
        assert!(self.def_name_span.insert(def_id, name.span()).is_none());

        if node_id != DUMMY_NODE_ID {
            assert!(self.node_id_def_id.insert(node_id, def_id).is_none());
            assert!(self.def_id_node_id.insert(def_id, node_id).is_none());
        }

        def_id
    }

    pub fn get_def(&self, def_id: DefId) -> &Def {
        self.defs.get(def_id.as_usize()).as_ref().unwrap()
    }

    pub fn get_def_id(&self, node_id: NodeId) -> Option<DefId> {
        self.node_id_def_id.get_flat(node_id).copied()
    }

    pub fn get_node_id(&self, def_id: DefId) -> Option<NodeId> {
        self.def_id_node_id.get_flat(def_id).copied()
    }

    pub fn defs(&self) -> &[Def] {
        self.defs.as_ref()
    }

    pub fn defs_count(&self) -> usize {
        self.defs.len()
    }

    pub fn blocks(&self) -> &NodeMap<Module> {
        &self.blocks
    }

    pub fn node_id_def_id(&self) -> &NodeMap<DefId> {
        &self.node_id_def_id
    }

    pub fn def_id_node_id(&self) -> &DefMap<NodeId> {
        &self.def_id_node_id
    }

    pub fn builtin_func(&self) -> DeclareBuiltin {
        self.declare_builtin.unwrap()
    }

    pub fn set_declare_builtin(&mut self, declare_builtin_def_id: DeclareBuiltin) {
        assert!(self.declare_builtin.is_none());
        self.declare_builtin = Some(declare_builtin_def_id);
    }

    pub fn add_builtin(&mut self, builtin: Builtin, def_id: DefId) {
        assert!(self.builtins.insert(builtin, def_id).is_none());
        assert!(self.def_id_builtins.insert(def_id, builtin).is_none());
    }

    pub fn as_builtin(&self, def_id: DefId) -> Option<Builtin> {
        self.def_id_builtins.get_flat(def_id).copied()
    }

    pub fn builtin(&self, bt: Builtin) -> DefId {
        self.builtins.get(&bt).copied().unwrap()
    }

    pub fn root_span(&self) -> Span {
        self.def_name_span.get(&ROOT_DEF_ID).copied().unwrap()
    }

    pub fn def_name_span(&self, def_id: DefId) -> Span {
        // FIXME: Span always exists?
        self.def_name_span
            .get(&def_id)
            .copied()
            .expect(&format!("Cannot find name span of def{}", def_id))
    }

    pub fn main_func(&mut self) -> Result<DefId, MessageBuilder> {
        if let Some(main_func) = self.main_func {
            return Ok(main_func);
        }

        let root_mod = self.modules.get_unwrap(ROOT_DEF_ID);
        let main = root_mod.get_from_ns(Namespace::Value, &Ident::synthetic("main".intern()));

        match main {
            Some(def_id) => {
                let def = self.get_def(def_id);
                match def.kind() {
                    DefKind::Func => {
                        self.main_func = Some(def_id);
                        Ok(def_id)
                    },
                    _ => Err(MessageBuilder::error()
                        .span(self.def_name_span(def_id))
                        .text(format!(
                            "'main' function is not defined, but {} named 'main' found",
                            def.kind()
                        ))
                        .label(
                            self.def_name_span(def_id),
                            format!(
                                "Consider renaming {} and define 'main' function",
                                def.kind()
                            ),
                        )),
                }
            },
            None => Err(MessageBuilder::error()
                .span(self.root_span())
                .text(format!("'main' function is not defined"))
                .label(self.root_span(), format!("Define function 'main'"))),
        }
    }

    pub fn expect_main_func(&self) -> DefId {
        self.main_func.unwrap()
    }
}
