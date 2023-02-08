use std::fmt::Display;

use crate::{
    ast::visitor::walk_each_pr,
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, IndexVec},
    span::span::{Ident, Span, WithSpan},
};

use self::{
    expr::{Block, Call, Expr, ExprKind, Infix, Lambda, Lit, PathExpr, TyExpr},
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

pub struct AstMetadata {
    last_node_index: NodeId,
}

impl AstMetadata {
    pub fn new() -> Self {
        Self {
            last_node_index: ROOT_NODE_ID.next(),
        }
    }

    pub fn next_node_id(&mut self) -> NodeId {
        let id = self.last_node_index;
        self.last_node_index.inc();
        id
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
}

impl Display for PathSeg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", pr_display(&self.name))
    }
}

impl WithSpan for PathSeg {
    fn span(&self) -> Span {
        self.span
    }
}

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

impl WithSpan for Path {
    fn span(&self) -> Span {
        self.span
    }
}

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

    fn visit_expr_stmt(&mut self, expr: &'ast PR<N<Expr>>) {
        walk_pr!(self, expr, visit_expr)
    }

    fn visit_item_stmt(&mut self, item: &'ast PR<N<Item>>) {
        walk_pr!(self, item, visit_item)
    }

    fn visit_item(&mut self, item: &'ast Item) {
        self.map.map.insert(item.id(), AstNode::Item(item));
        match item.kind() {
            item::ItemKind::Type(name, ty) => self.visit_type_item(name, ty, item.id()),
            item::ItemKind::Mod(name, items) => self.visit_mod_item(name, items, item.id()),
            item::ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id())
            },
        }
    }

    fn visit_type_item(&mut self, name: &'ast PR<Ident>, ty: &'ast PR<N<Ty>>, _: NodeId) {
        walk_pr!(self, name, visit_ident);
        walk_pr!(self, ty, visit_ty);
    }

    fn visit_mod_item(&mut self, name: &'ast PR<Ident>, items: &'ast Vec<PR<N<Item>>>, _: NodeId) {
        walk_pr!(self, name, visit_ident);
        walk_each_pr!(self, items, visit_item);
    }

    fn visit_decl_item(
        &mut self,
        name: &'ast PR<Ident>,
        params: &'ast Vec<PR<Pat>>,
        body: &'ast PR<N<Expr>>,
        _: NodeId,
    ) {
        walk_pr!(self, name, visit_ident);
        walk_each_pr!(self, params, visit_pat);
        walk_pr!(self, body, visit_expr);
    }

    fn visit_pat(&mut self, pat: &'ast Pat) {
        self.map.map.insert(pat.id(), AstNode::Pat(pat));
        match pat.kind() {
            pat::PatKind::Ident(ident) => walk_pr!(self, ident, visit_ident_pat),
        }
    }

    fn visit_ident_pat(&mut self, ident: &'ast Ident) {
        self.visit_ident(ident);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        self.map.map.insert(expr.id(), AstNode::Expr(expr));
        match expr.kind() {
            ExprKind::Unit => self.visit_unit_expr(),
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Paren(inner) => walk_pr!(self, inner, visit_expr),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(infix) => self.visit_infix_expr(infix),
            ExprKind::Call(call) => self.visit_app_expr(call),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Lambda(lambda) => self.visit_lambda_expr(lambda),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr),
        }
    }

    fn visit_unit_expr(&mut self) {}

    fn visit_lit_expr(&mut self, _: &'ast Lit) {}

    fn visit_path_expr(&mut self, path: &'ast PathExpr) {
        walk_pr!(self, &path.0, visit_path)
    }

    fn visit_block_expr(&mut self, block: &'ast PR<Block>) {
        walk_pr!(self, block, visit_block)
    }

    fn visit_infix_expr(&mut self, infix: &'ast Infix) {
        walk_pr!(self, &infix.lhs, visit_expr);
        walk_pr!(self, &infix.rhs, visit_expr);
    }

    fn visit_app_expr(&mut self, call: &'ast Call) {
        walk_pr!(self, &call.lhs, visit_expr);
        walk_pr!(self, &call.arg, visit_expr);
    }

    fn visit_lambda_expr(&mut self, lambda: &'ast Lambda) {
        walk_pr!(self, &lambda.param, visit_pat);
        walk_pr!(self, &lambda.body, visit_expr);
    }

    fn visit_let_expr(&mut self, block: &'ast PR<Block>) {
        walk_pr!(self, block, visit_block)
    }

    fn visit_type_expr(&mut self, ty_expr: &'ast TyExpr) {
        walk_pr!(self, &ty_expr.expr, visit_expr);
        walk_pr!(self, &ty_expr.ty, visit_ty);
    }

    fn visit_ty(&mut self, ty: &'ast Ty) {
        self.map.map.insert(ty.id(), AstNode::Ty(ty));
        match ty.kind() {
            ty::TyKind::Unit => self.visit_unit_ty(),
            ty::TyKind::Path(path) => self.visit_path_ty(path),
            ty::TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
            ty::TyKind::Paren(inner) => self.visit_paren_ty(inner),
        }
    }

    fn visit_unit_ty(&mut self) {}

    fn visit_path_ty(&mut self, path: &'ast PR<Path>) {
        walk_pr!(self, path, visit_path)
    }

    fn visit_func_ty(&mut self, param_ty: &'ast PR<N<Ty>>, return_ty: &'ast PR<N<Ty>>) {
        walk_pr!(self, param_ty, visit_ty);
        walk_pr!(self, return_ty, visit_ty)
    }

    fn visit_paren_ty(&mut self, inner: &'ast PR<N<Ty>>) {
        walk_pr!(self, inner, visit_ty)
    }

    fn visit_ident(&mut self, _: &'ast Ident) {}

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
