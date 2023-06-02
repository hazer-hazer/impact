use super::{
    expr::{Block, Expr, ExprKind},
    item::{self, Item},
    pat::{self, Pat},
    stmt::{self, Stmt},
    ty::{self, Ty},
    visitor::{walk_pr, AstVisitor},
    ErrorNode, NodeId, NodeMap, Path, WithNodeId, AST,
};
use crate::ast::visitor::walk_each_pr;

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

// TODO: Remove 'cause unused
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
            item::ItemKind::Type(name, generics, ty) => {
                self.visit_ty_item(name, generics, ty, item.id())
            },
            item::ItemKind::Mod(name, items) => self.visit_mod_item(name, items, item.id()),
            item::ItemKind::Decl(name, params, body) => {
                self.visit_decl_item(name, params, body, item.id())
            },
            item::ItemKind::Adt(name, generics, variants) => {
                self.visit_adt_item(name, generics, variants, item.id())
            },
            item::ItemKind::Struct(name, generics, fields) => {
                self.visit_struct_item(name, generics, fields)
            },
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
