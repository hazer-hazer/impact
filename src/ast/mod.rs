use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::visitor::walk_each_pr,
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, IndexVec},
    parser::token::{Token, TokenKind},
    span::span::{impl_with_span, Ident, Span, Symbol, WithSpan},
};

use self::{
    expr::{Block, Expr, ExprKind},
    item::Item,
    pat::Pat,
    stmt::Stmt,
    ty::Ty,
    visitor::{walk_pr, AstVisitor},
};

pub mod expr;
pub mod item;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod validator;
pub mod visitor;

pub type N<T> = Box<T>;

pub type PR<T> = Result<T, ErrorNode>;

impl<T> WithNodeId for Result<T, ErrorNode>
where
    T: WithNodeId,
{
    fn id(&self) -> NodeId {
        match self {
            Ok(node) => node.id(),
            Err(err) => err.id(),
        }
    }
}

pub fn pr_display<T>(pr: &PR<T>) -> String
where
    T: Display,
{
    match pr {
        Ok(ok) => format!("{}", ok),
        Err(err) => format!("{}", err),
    }
}

pub fn prs_display_join<T>(prs: &[PR<T>], sep: &str) -> String
where
    T: Display,
{
    prs.iter().map(pr_display).collect::<Vec<_>>().join(sep)
}

pub fn pr_node_kind_str<T>(pr: &PR<T>) -> String
where
    T: NodeKindStr,
{
    match pr {
        Ok(ok) => ok.kind_str(),
        Err(_) => "I don't what is that...".to_string(),
    }
}

impl<T> WithSpan for PR<T>
where
    T: WithSpan,
{
    fn span(&self) -> Span {
        match self {
            Ok(ok) => ok.span(),
            Err(err) => err.span,
        }
    }
}

declare_idx!(NodeId, u32, "#{}", Color::Blue);

pub const ROOT_NODE_ID: NodeId = NodeId(0);
pub const DUMMY_NODE_ID: NodeId = NodeId(u32::MAX);

pub trait WithNodeId {
    fn id(&self) -> NodeId;
}

pub type NodeMap<T> = IndexVec<NodeId, Option<T>>;

pub struct AST {
    items: Vec<PR<N<Item>>>,
}

impl AST {
    pub fn new(items: Vec<PR<N<Item>>>) -> Self {
        Self { items }
    }

    pub fn items(&self) -> &[PR<N<Item>>] {
        self.items.as_ref()
    }
}

impl WithNodeId for AST {
    fn id(&self) -> NodeId {
        ROOT_NODE_ID
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReservedNodeId {
    DeclareBuiltin,
}

pub struct AstMetadata {
    last_node_index: NodeId,
    reserved: HashMap<ReservedNodeId, NodeId>,
}

impl AstMetadata {
    pub fn new() -> Self {
        Self {
            last_node_index: ROOT_NODE_ID.next(),
            reserved: Default::default(),
        }
    }

    pub fn next_node_id(&mut self) -> NodeId {
        let id = self.last_node_index;
        self.last_node_index.inc();
        id
    }

    pub fn reserve(&mut self, kind: ReservedNodeId) -> NodeId {
        let node_id = self.next_node_id();
        assert!(self.reserved.insert(kind, node_id).is_none());
        node_id
    }
}

#[derive(Debug, Clone)]
pub struct ErrorNode {
    span: Span,
    parsed: Option<String>,
}

impl ErrorNode {
    pub fn new(span: Span) -> Self {
        Self { span, parsed: None }
    }

    pub fn new_parsed<T>(node: T) -> Self
    where
        T: WithSpan + Display,
    {
        Self {
            span: node.span(),
            parsed: Some(format!("{}", node)),
        }
    }

    pub fn parsed(&self) -> Option<&String> {
        self.parsed.as_ref()
    }
}

impl Display for ErrorNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ERROR]")
    }
}

impl WithNodeId for ErrorNode {
    fn id(&self) -> NodeId {
        return DUMMY_NODE_ID;
    }
}

#[derive(Debug, Clone)]
pub struct IdentNode {
    pub id: NodeId,
    pub ident: Ident,
}

impl IdentNode {
    pub fn new(id: NodeId, ident: Ident) -> Self {
        Self { id, ident }
    }
}

impl WithNodeId for IdentNode {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl Display for IdentNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ident.fmt(f)
    }
}

/// Give some pretty name for node-like structure in errors 😺
pub trait NodeKindStr {
    fn kind_str(&self) -> String;
}

impl<T> NodeKindStr for Box<T>
where
    T: NodeKindStr,
{
    fn kind_str(&self) -> String {
        (**self).kind_str()
    }
}

#[derive(Debug, Clone)]
pub struct PathSeg {
    name: PR<Ident>,
    span: Span,
}

impl PathSeg {
    pub fn new(name: PR<Ident>, span: Span) -> Self {
        Self { name, span }
    }

    pub fn expect_name(&self) -> &Ident {
        self.name
            .as_ref()
            .expect("Error PathSeg::name in `PathSeg::expect_name`")
    }

    pub fn is_from_ty_ns(&self) -> bool {
        self.name.as_ref().map_or(false, |name| name.is_ty())
    }
}

impl Display for PathSeg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pr_display(&self.name))
    }
}

impl_with_span!(PathSeg);

#[derive(Debug, Clone)]
pub struct Path {
    id: NodeId,
    segments: Vec<PathSeg>,
    span: Span,
}

impl Path {
    pub fn new(id: NodeId, segments: Vec<PathSeg>, span: Span) -> Self {
        Self { id, segments, span }
    }

    pub fn new_infix_op(id: NodeId, tok: Token) -> Self {
        let segments = vec![PathSeg::new(
            Ok(Ident::new(
                tok.span(),
                match tok.kind {
                    TokenKind::Op(op) => Symbol::intern(&op.to_string()),
                    TokenKind::CustomOp(op) => op,
                    _ => panic!(),
                },
            )),
            tok.span,
        )];
        Self {
            id,
            segments,
            span: tok.span,
        }
    }

    pub fn prefix_str(&self, to: usize) -> String {
        let prefix = self.segments()[0..to]
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(".");

        if prefix.is_empty() {
            "current scope".to_string()
        } else {
            prefix
        }
    }

    pub fn original_str(&self) -> String {
        self.segments()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(".")
    }

    pub fn prefix_span(&self, to: usize) -> Span {
        self.segments()[0..=to]
            .iter()
            .map(|seg| seg.span())
            .reduce(|prefix, seg| prefix.to(seg))
            .expect("Empty Path in `Path::prefix_span`")
    }

    pub fn target_name(&self) -> Ident {
        *self
            .segments()
            .last()
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .unwrap()
    }

    pub fn segments(&self) -> &[PathSeg] {
        self.segments.as_ref()
    }
}

impl WithNodeId for Path {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl_with_span!(Path);

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.segments()
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

pub trait IsBlockEnded {
    fn is_block_ended(&self) -> bool;
}

macro_rules! is_block_ended {
    ($pr: expr) => {
        match &$pr {
            Err(_) => false,
            Ok(node) => node.is_block_ended(),
        }
    };
}

pub(crate) use is_block_ended;

// Referencing ADT to any AST node, where node is anything with `NodeId`
pub enum AstNode<'ast> {
    Path(&'ast Path),
    Block(&'ast Block),
    Expr(&'ast Expr),
    Item(&'ast Item),
    Pat(&'ast Pat),
    Stmt(&'ast Stmt),
    Ty(&'ast Ty),
}

#[derive(Default)]
pub struct AstMap<'ast> {
    map: NodeMap<AstNode<'ast>>,
}

macro_rules! impl_ast_map_get {
    ($($method: ident $type: tt),*) => {
        impl<'ast> AstMap<'ast> {
            $(
                pub fn $method(&self, id: NodeId) -> &$type {
                    match self.map.get(id) {
                        Some(node) => {
                            match node.as_ref().unwrap() {
                                AstNode::$type(node) => node,
                                _ => panic!()
                            }
                        },
                        None => panic!("Failed to find node by NodeId{}", id),
                    }
                }
            )*
        }
    };
}

impl_ast_map_get!(
    path Path,
    block Block,
    expr Expr,
    item Item,
    pat Pat,
    stmt Stmt,
    ty Ty
);

pub struct AstMapFiller<'ast> {
    map: AstMap<'ast>,
}

impl<'ast> AstMapFiller<'ast> {
    pub fn new() -> Self {
        Self {
            map: Default::default(),
        }
    }

    pub fn fill(mut self, ast: &'ast AST) -> AstMap<'ast> {
        self.visit_ast(&ast);
        self.map
    }
}

pub struct MappedAst<'ast> {
    ast: &'ast AST,
    map: AstMap<'ast>,
}

impl<'ast> MappedAst<'ast> {
    pub fn new(ast: &'ast AST, map: AstMap<'ast>) -> Self {
        Self { ast, map }
    }

    pub fn map(&self) -> &AstMap<'ast> {
        &self.map
    }

    pub fn ast(&self) -> &AST {
        self.ast
    }
}

impl<'ast> AstVisitor<'ast> for AstMapFiller<'ast> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        self.map.map.insert(stmt.id(), AstNode::Stmt(stmt));
        match stmt.kind() {
            stmt::StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            stmt::StmtKind::Item(item) => self.visit_item_stmt(item),
        }
    }

    fn visit_item(&mut self, item: &'ast Item) {
        self.map.map.insert(item.id(), AstNode::Item(item));
        match item.kind() {
            item::ItemKind::Type(name, ty) => self.visit_type_item(name, ty, item.id()),
            item::ItemKind::Mod(name, items) => self.visit_mod_item(name, items, item.id()),
            item::ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id())
            },
            item::ItemKind::Data(name, variants) => self.visit_data_item(name, variants, item.id()),
            item::ItemKind::Extern(items) => self.visit_extern_block(items),
        }
    }

    fn visit_pat(&mut self, pat: &'ast Pat) {
        self.map.map.insert(pat.id(), AstNode::Pat(pat));
        match pat.kind() {
            pat::PatKind::Unit => self.visit_unit_pat(),
            pat::PatKind::Ident(ident) => walk_pr!(self, ident, visit_ident_pat),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        self.map.map.insert(expr.id(), AstNode::Expr(expr));
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Paren(inner) => walk_pr!(self, inner, visit_expr),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(infix) => self.visit_infix_expr(infix),
            ExprKind::Call(call) => self.visit_app_expr(call),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Lambda(lambda) => self.visit_lambda_expr(lambda),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr),
            ExprKind::DotOp(expr, field) => self.visit_dot_op_expr(expr, field),
        }
    }

    fn visit_ty(&mut self, ty: &'ast Ty) {
        self.map.map.insert(ty.id(), AstNode::Ty(ty));
        match ty.kind() {
            ty::TyKind::Path(path) => self.visit_ty_path(path),
            ty::TyKind::Func(params, body) => self.visit_func_ty(params, body),
            ty::TyKind::Paren(inner) => self.visit_paren_ty(inner),
            ty::TyKind::App(cons, args) => self.visit_ty_app(cons, args),
            ty::TyKind::AppExpr(cons, args) => self.visit_ty_app_expr(cons, args),
        }
    }

    fn visit_path(&mut self, path: &'ast Path) {
        self.map.map.insert(path.id(), AstNode::Path(path));
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.map.map.insert(block.id(), AstNode::Block(block));
        walk_each_pr!(self, block.stmts(), visit_stmt);
    }

    fn visit_err(&mut self, _: &'ast ErrorNode) {
        todo!()
    }
}

//
