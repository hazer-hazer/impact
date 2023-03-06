/*!
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
use core::panic;
use std::{collections::HashMap, fmt::Display};

use crate::{
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, Idx, IndexVec},
    resolve::{
        builtin::Builtin,
        def::{DefId, DefKind, DefMap, ROOT_DEF_ID},
    },
    span::span::{Ident, Span, Symbol, WithSpan},
};

use self::{
    expr::{Block, BlockNode, Expr, ExprKind, ExprNode, Lambda},
    item::{ItemId, ItemKind, ItemNode, Mod},
    pat::{Pat, PatNode},
    stmt::StmtNode,
    ty::TyNode,
};

pub mod expr;
pub mod item;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod visitor;

type N<T> = Box<T>;

declare_idx!(OwnerId, DefId, "{}", Color::BrightCyan);
declare_idx!(wrapper BodyId, HirId, "body{}", Color::Green);
declare_idx!(OwnerChildId, u32, "#{}", Color::Cyan);

pub const ROOT_OWNER_ID: OwnerId = OwnerId(ROOT_DEF_ID);
pub const OWNER_SELF_CHILD_ID: OwnerChildId = OwnerChildId(0);
pub const FIRST_OWNER_CHILD_ID: OwnerChildId = OwnerChildId(1);

type OwnerChildrenMap<T> = IndexVec<OwnerChildId, Option<T>>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct HirId {
    owner: OwnerId,
    id: OwnerChildId,
}

impl HirId {
    pub fn new(owner: OwnerId, id: OwnerChildId) -> Self {
        Self { owner, id }
    }

    pub fn new_owner(def_id: DefId) -> Self {
        Self {
            owner: OwnerId(def_id),
            id: OwnerChildId(0),
        }
    }

    pub fn as_owner(&self) -> Option<OwnerId> {
        if self.is_owner() {
            Some(self.owner)
        } else {
            None
        }
    }

    pub fn is_owner(&self) -> bool {
        self.id.as_usize() == 0
    }

    pub fn owner(&self) -> OwnerId {
        self.owner
    }

    pub fn child_id(&self) -> OwnerChildId {
        self.id
    }
}

impl Display for HirId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}]", self.owner, self.id)
    }
}

pub type HirMap<T> = HashMap<HirId, T>;

pub trait WithHirId {
    fn id(&self) -> HirId;
}

#[derive(Debug)]
pub struct ErrorNode {
    pub id: HirId,
    pub span: Span,
}

// HIR Node is any Node in HIR with HirId
macro_rules! hir_nodes {
    // identified by HirId / other
    ($($name: ident $ty: tt,)* / $($other_name: ident $other_ty: tt,)*) => {
        #[derive(Debug)]
        pub enum Node {
            $(
                $ty($ty),
            )*
            $(
                $other_ty($other_ty),
            )*
        }

        impl Node {
            pub fn name(&self) -> &str {
                match self {
                    $(Self::$ty(_) => stringify!($name),)*
                    $(Self::$other_ty(_) => stringify!($other_name),)*
                }
            }

            $(
                pub fn $name(&self) -> &$ty {
                    match self {
                        Self::$ty(inner) => inner,
                        _ => panic!(),
                    }
                }
            )*

            $(
                pub fn $other_name(&self) -> &$other_ty {
                    match self {
                        Self::$other_ty(inner) => inner,
                        _ => panic!(),
                    }
                }
            )*
        }

        impl<'hir> HIR {
            $(
                pub fn $name(&self, $name: HirId) -> &$ty {
                    match self.node($name) {
                        Node::$ty(node) => node,
                        n @ _ => panic!("Expected {} HIR node, got {}", stringify!($name), n.name()),
                    }
                }
            )*
        }
    };
}

hir_nodes!(
    expr ExprNode,
    stmt StmtNode,
    pat PatNode,
    block BlockNode,
    ty TyNode,
    path PathNode,
    /
    item ItemNode,
    root Mod,
    error ErrorNode,
);

/// Do not confuse with `Node`, this is just kind of list of names.
// Note: Keep referencing `Node` names
pub enum NodeKind {
    Expr,
    Stmt,
    Pat,
    Block,
    Ty,
    Path,
    Item,
    Root,
    Error,
}

impl Display for NodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeKind::Expr => "expr",
            NodeKind::Stmt => "stmt",
            NodeKind::Pat => "pat",
            NodeKind::Block => "block",
            NodeKind::Ty => "ty",
            NodeKind::Path => "path",
            NodeKind::Item => "item",
            NodeKind::Root => "root",
            NodeKind::Error => "[ERROR]",
        }
        .fmt(f)
    }
}

pub trait WithNodeKind {
    fn kind(&self) -> NodeKind;
}

// TODO: impl WithNodeKind for Node

macro_rules! impl_with_node_kind {
    ($($ty: ty: $kind: expr),*) => {
        $(
            impl WithNodeKind for $ty {
                fn kind(&self) -> NodeKind {
                    $kind
                }
            }
        )*
    };
}

impl_with_node_kind!(
    ExprNode: NodeKind::Expr,
    StmtNode: NodeKind::Stmt,
    PatNode: NodeKind::Pat,
    BlockNode: NodeKind::Block,
    TyNode: NodeKind::Ty,
    PathNode: NodeKind::Path,
    ItemNode: NodeKind::Item,
    Mod: NodeKind::Root,
    ErrorNode: NodeKind::Error
);

impl Node {
    pub fn as_owner<'hir>(&'hir self) -> Option<OwnerNode<'hir>> {
        match self {
            Node::ItemNode(item) => Some(OwnerNode::Item(item)),
            Node::Mod(root) => Some(OwnerNode::Root(root)),
            _ => None,
        }
    }

    pub fn hir_id(&self) -> HirId {
        match self {
            Node::ExprNode(expr) => expr.id(),
            Node::StmtNode(stmt) => stmt.id(),
            Node::PatNode(pat) => pat.id(),
            Node::BlockNode(block) => block.id(),
            Node::TyNode(ty) => ty.id(),
            Node::PathNode(path) => path.id(),
            Node::ItemNode(item) => HirId::new_owner(item.def_id()),
            Node::Mod(_root) => HirId::new_owner(ROOT_DEF_ID),
            Node::ErrorNode(error) => error.id,
        }
    }

    pub fn kind(&self) -> NodeKind {
        match self {
            Node::ExprNode(_) => NodeKind::Expr,
            Node::StmtNode(_) => NodeKind::Stmt,
            Node::PatNode(_) => NodeKind::Pat,
            Node::BlockNode(_) => NodeKind::Block,
            Node::TyNode(_) => NodeKind::Ty,
            Node::PathNode(_) => NodeKind::Path,
            Node::ItemNode(_) => NodeKind::Item,
            Node::Mod(_) => NodeKind::Root,
            Node::ErrorNode(_) => NodeKind::Error,
        }
    }
}

pub enum OwnerNode<'hir> {
    Root(&'hir Mod),
    Item(&'hir ItemNode),
}

#[derive(Default, Debug)]
pub struct Owner {
    pub bodies: OwnerChildrenMap<Body>,

    // OwnerNode is the first child of nodes
    pub nodes: OwnerChildrenMap<Node>,
}

impl Owner {
    pub fn owner_node(&self) -> OwnerNode {
        self.nodes
            .get_expect(OWNER_SELF_CHILD_ID, "No owner self node in owner nodes")
            .as_owner()
            .expect("First node in owner nodes is non-owner node")
    }
}

#[derive(Debug)]
pub struct HIR {
    // TODO: Replace with IndexVec
    // Optimize: Late init IndexVec::with_capacity count of definitions in DefTable
    owners: DefMap<Owner>,
}

impl HIR {
    pub fn new() -> Self {
        Self {
            owners: Default::default(),
        }
    }

    pub fn add_owner(&mut self, def_id: DefId, owner: Owner) {
        self.owners.insert(def_id, owner);
    }

    // Getters //
    pub fn expect_owner(&self, def_id: DefId) -> &Owner {
        self.owners
            .get_expect(def_id, &format!("Expect owner {}", def_id))
    }

    pub fn expect_owner_node(&self, def_id: DefId) -> OwnerNode {
        self.expect_owner(def_id).owner_node()
    }

    pub fn root(&self) -> &Mod {
        match self.expect_owner_node(ROOT_DEF_ID) {
            OwnerNode::Root(root) => root,
            OwnerNode::Item(_) => unreachable!(),
        }
    }

    pub fn item(&self, item_id: ItemId) -> &ItemNode {
        match self.owners.get_unwrap(item_id.inner().inner()).owner_node() {
            OwnerNode::Item(item) => item,
            OwnerNode::Root(_) => todo!(),
        }
    }

    pub fn string_lit_value(&self, expr: Expr) -> Option<Symbol> {
        match self.expr(expr).kind() {
            ExprKind::Lit(lit) => match lit {
                &expr::Lit::String(val) => Some(val),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn node(&self, id: HirId) -> &Node {
        self.owners
            .get_unwrap(id.owner.inner())
            .nodes
            .get_unwrap(id.id)
    }

    pub fn body(&self, id: BodyId) -> &Body {
        self.owners
            .get_unwrap(id.0.owner.inner())
            .bodies
            .get_unwrap(id.0.id)
    }

    pub fn body_value(&self, id: BodyId) -> Expr {
        self.body(id).value
    }

    pub fn owner_body(&self, owner_id: OwnerId) -> Option<BodyId> {
        match self.node(HirId::new_owner(owner_id.into())) {
            Node::ExprNode(ExprNode {
                kind: ExprKind::Lambda(Lambda { body, .. }),
                ..
            }) => Some(*body),
            Node::ItemNode(ItemNode {
                kind: ItemKind::Func(body),
                ..
            }) => Some(*body),
            _ => None,
        }
    }

    pub fn body_owner_kind(&self, owner_id: OwnerId) -> BodyOwnerKind {
        match self.node(HirId::new_owner(owner_id.into())) {
            Node::ExprNode(ExprNode {
                kind: ExprKind::Lambda(Lambda { .. }),
                ..
            }) => BodyOwnerKind::Lambda,
            Node::ItemNode(ItemNode {
                kind: ItemKind::Func(_),
                ..
            }) => BodyOwnerKind::Func,
            _ => panic!(),
        }
    }

    pub fn pat_names(&self, pat: Pat) -> Option<Vec<Ident>> {
        match self.pat(pat).kind() {
            pat::PatKind::Unit => None,
            &pat::PatKind::Ident(name) => Some(vec![name]),
        }
    }

    pub fn expr_result_span(&self, expr_id: Expr) -> Span {
        let expr = self.expr(expr_id);
        match expr.kind() {
            ExprKind::Lit(_)
            | ExprKind::Path(_)
            | ExprKind::Lambda(_)
            | ExprKind::Call(_)
            | ExprKind::BuiltinExpr(_)
            | ExprKind::Ty(_) => expr.span(),
            &ExprKind::Block(block) | &ExprKind::Let(block) => self.block_result_span(block),
        }
    }

    pub fn block_result_span(&self, block: Block) -> Span {
        let block = self.block(block);

        block.expr().map_or_else(
            || {
                self.stmt(*block.stmts().last().unwrap())
                    .span()
                    .point_after_hi()
            },
            |&expr| self.expr_result_span(expr),
        )
    }

    pub fn return_ty_span(&self, def_id: DefId) -> Span {
        self.pat(self.body(self.owner_body(def_id.into()).unwrap()).param)
            .span()
            .point_after_hi()
        // match self.node(HirId::new_owner(def_id)) {
        //     Node::ExprNode(expr) => match expr.kind() {
        //         ExprKind::Lambda(Lambda { body }) => todo!(),
        //         _ => panic!(),
        //     },
        //     Node::ItemNode(item) => todo!(),
        //     _ => panic!(),
        // }
    }

    // // Debug //
    // pub fn dump(&self) -> String {
    //     let dump_owner = |owner: &Owner| -> String {

    //     };

    //     self.owners
    //         .iter()
    //         .map(|(def_id, owner)| format!("{}: {}", def_id, dump_owner(owner)))
    //         .collect::<Vec<_>>()
    //         .join("\n")
    // }
}

#[derive(Clone, Copy)]
pub enum BodyOwnerKind {
    Func,
    Lambda,
}

#[derive(Debug)]
pub struct Body {
    pub param: Pat,
    pub value: Expr,
}

impl Body {
    pub fn new(param: Pat, value: Expr) -> Self {
        Self { param, value }
    }
}

#[derive(Debug)]
pub struct PathSeg {
    ident: Ident,
    span: Span,
}

impl PathSeg {
    pub fn new(ident: Ident, span: Span) -> Self {
        Self { ident, span }
    }
}

impl Display for PathSeg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Res {
    Def(DefKind, DefId),
    Local(HirId),
    DeclareBuiltin,
    Builtin(Builtin),
    Error,
}

impl Display for Res {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Res::Def(kind, id) => write!(f, "{} def {}", kind, id),
            Res::Local(hir_id) => write!(f, "local {}", hir_id),
            Res::DeclareBuiltin => write!(f, "[`builtin`]"),
            Res::Builtin(bt) => write!(f, "[builtin {}]", bt),
            Res::Error => write!(f, "[ERROR]"),
        }
    }
}

#[derive(Debug)]
pub struct PathNode {
    id: HirId,
    res: Res,
    segments: Vec<PathSeg>,
    span: Span,
}

impl PathNode {
    pub fn new(id: HirId, res: Res, segments: Vec<PathSeg>, span: Span) -> Self {
        Self {
            id,
            res,
            segments,
            span,
        }
    }

    pub fn res(&self) -> &Res {
        &self.res
    }

    pub fn segments(&self) -> &[PathSeg] {
        self.segments.as_ref()
    }

    pub fn target_name(&self) -> Ident {
        self.segments.last().unwrap().ident
    }
}

impl WithHirId for PathNode {
    fn id(&self) -> HirId {
        self.id
    }
}

impl Display for PathNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.segments()
                .iter()
                .map(|seg| format!("{}", seg))
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

impl WithSpan for PathNode {
    fn span(&self) -> Span {
        self.span
    }
}

pub type Path = HirId;
