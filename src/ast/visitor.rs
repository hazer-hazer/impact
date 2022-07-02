use crate::span::span::Ident;

use super::{
    expr::{Block, Expr, ExprKind, InfixOp, Lit, PrefixOp},
    item::{Item, ItemKind},
    stmt::{Stmt, StmtKind},
    ty::{LitTy, Ty, TyKind},
    ErrorNode, Path, AST, N, PR,
};

macro_rules! visit_pr {
    ($self: ident, $pr: expr, $ok_visitor: ident) => {
        match $pr {
            Ok(ok) => $self.$ok_visitor(ok),
            Err(err) => $self.visit_err(err),
        }
    };
}

pub(crate) use visit_pr;

macro_rules! visit_each_pr {
    ($self: ident, $prs: expr, $ok_visitor: ident) => {
        for pr in $prs {
            match pr {
                Ok(ok) => $self.$ok_visitor(ok),
                Err(err) => $self.visit_err(err),
            }
        }
    };
}

pub(crate) use visit_each_pr;

pub trait AstVisitor<T> {
    fn visit_err(&self, _: &ErrorNode) -> T;

    fn visit_ast(&mut self, ast: &AST) -> T;

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) -> T {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &PR<N<Expr>>) -> T {
        visit_pr!(self, expr, visit_expr)
    }

    fn visit_item_stmt(&mut self, item: &PR<N<Item>>) -> T {
        visit_pr!(self, item, visit_item)
    }

    // Items //
    fn visit_item(&mut self, item: &Item) -> T {
        match item.kind() {
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty),
            ItemKind::Mod(name, items) => self.visit_mod_item(name, items),
            ItemKind::Decl(name, params, body) => self.visit_decl_item(name, params, body),
        }
    }

    fn visit_type_item(&mut self, name: &PR<Ident>, ty: &PR<N<Ty>>) -> T;

    fn visit_mod_item(&mut self, name: &PR<Ident>, items: &Vec<PR<N<Item>>>) -> T;

    fn visit_decl_item(
        &mut self,
        name: &PR<Ident>,
        params: &Vec<PR<Ident>>,
        body: &PR<N<Expr>>,
    ) -> T;

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) -> T {
        match expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Infix(lhs, op, rhs) => self.visit_infix_expr(lhs, op, rhs),
            ExprKind::Prefix(op, rhs) => self.visit_prefix_expr(op, rhs),
            ExprKind::App(lhs, arg) => self.visit_app_expr(lhs, arg),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Abs(param, body) => self.visit_abs_expr(param, body),
            ExprKind::Ty(expr, ty) => self.visit_type_expr(expr, ty),
        }
    }

    fn visit_lit_expr(&mut self, lit: &Lit) -> T;
    fn visit_path_expr(&mut self, path: &PR<Path>) -> T {
        visit_pr!(self, path, visit_path)
    }
    fn visit_infix_expr(&mut self, lhs: &PR<N<Expr>>, op: &InfixOp, rhs: &PR<N<Expr>>) -> T;
    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) -> T;
    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) -> T;
    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) -> T;
    fn visit_let_expr(&mut self, block: &PR<Block>) -> T;
    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) -> T;

    // Types //
    fn visit_ty(&mut self, ty: &Ty) -> T {
        match ty.kind() {
            TyKind::Lit(lit_ty) => self.visit_lit_ty(lit_ty),
            TyKind::Unit => self.visit_unit_ty(),
            TyKind::Path(path) => self.visit_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => self.visit_paren_ty(inner),
        }
    }

    fn visit_unit_ty(&mut self) -> T;
    fn visit_lit_ty(&mut self, lit_ty: &LitTy) -> T;
    fn visit_path_ty(&mut self, path: &PR<Path>) -> T {
        visit_pr!(self, path, visit_path)
    }
    fn visit_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) -> T;

    fn visit_paren_ty(&mut self, inner: &PR<N<Ty>>) -> T {
        visit_pr!(self, inner, visit_ty)
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) -> T;
    fn visit_path(&mut self, path: &Path) -> T;
    fn visit_block(&mut self, block: &Block) -> T;
}
