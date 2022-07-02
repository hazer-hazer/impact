use crate::span::span::Ident;

use super::{
    expr::{Block, Expr, ExprKind, InfixOp, Lit, PrefixOp},
    item::{Item, ItemKind},
    stmt::{Stmt, StmtKind},
    ty::{LitTy, Ty, TyKind},
    ErrorNode, Path, AST, N, PR,
};

macro_rules! walk_pr {
    ($self: ident, $pr: expr, $ok_visitor: ident) => {
        match $pr {
            Ok(ok) => $self.$ok_visitor(ok),
            Err(err) => $self.visit_err(err),
        }
    };
}

pub(crate) use walk_pr;

macro_rules! walk_each_pr {
    ($self: ident, $prs: expr, $ok_visitor: ident) => {
        for pr in $prs {
            match pr {
                Ok(ok) => $self.$ok_visitor(ok),
                Err(err) => $self.visit_err(err),
            }
        }
    };
}

pub(crate) use walk_each_pr;

/// This might be not a right solution, but AstVisitor as HirVisitor is a walker by default,
///  i.e it walks through all nodes as if visitors return unit type.
/// It may cause silent non-implemented problem in visitors with non-unit return type, but it allows
///  me to use visitors both for PP and walking in some stages where we don't need to implement visitor
///  for all nodes.
/// So keep in mind implementing all visitors you need or
pub trait AstVisitor {
    fn visit_err(&self, _: &ErrorNode);

    fn visit_ast(&mut self, ast: &AST) {
        walk_each_pr!(self, ast.items(), visit_item);
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &PR<N<Expr>>) {
        walk_pr!(self, expr, visit_expr)
    }

    fn visit_item_stmt(&mut self, item: &PR<N<Item>>) {
        walk_pr!(self, item, visit_item)
    }

    // Items //
    fn visit_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::Type(name, ty) => self.visit_type_item(name, ty),
            ItemKind::Mod(name, items) => self.visit_mod_item(name, items),
            ItemKind::Decl(name, params, body) => self.visit_decl_item(name, params, body),
        }
    }

    fn visit_type_item(&mut self, name: &PR<Ident>, ty: &PR<N<Ty>>) {
        walk_pr!(self, name, visit_ident);
        walk_pr!(self, ty, visit_ty);
    }

    fn visit_mod_item(&mut self, name: &PR<Ident>, items: &Vec<PR<N<Item>>>) {
        walk_pr!(self, name, visit_ident);
        walk_each_pr!(self, items, visit_item);
    }

    fn visit_decl_item(&mut self, name: &PR<Ident>, params: &Vec<PR<Ident>>, body: &PR<N<Expr>>) {
        walk_pr!(self, name, visit_ident);
        walk_each_pr!(self, params, visit_ident);
        walk_pr!(self, body, visit_expr);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) {
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

    fn visit_lit_expr(&mut self, lit: &Lit) {}

    fn visit_path_expr(&mut self, path: &PR<Path>) {
        walk_pr!(self, path, visit_path)
    }

    fn visit_infix_expr(&mut self, lhs: &PR<N<Expr>>, op: &InfixOp, rhs: &PR<N<Expr>>) {
        walk_pr!(self, lhs, visit_expr);
        walk_pr!(self, rhs, visit_expr);
    }

    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) {
        walk_pr!(self, rhs, visit_expr);
    }

    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) {
        walk_pr!(self, lhs, visit_expr);
        walk_pr!(self, arg, visit_expr);
    }

    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) {
        walk_pr!(self, param, visit_ident);
        walk_pr!(self, body, visit_expr);
    }

    fn visit_let_expr(&mut self, block: &PR<Block>) {
        walk_pr!(self, block, visit_block)
    }

    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) {
        walk_pr!(self, expr, visit_expr);
        walk_pr!(self, ty, visit_ty);
    }

    // Types //
    fn visit_ty(&mut self, ty: &Ty) {
        match ty.kind() {
            TyKind::Lit(lit_ty) => self.visit_lit_ty(lit_ty),
            TyKind::Unit => self.visit_unit_ty(),
            TyKind::Path(path) => self.visit_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => self.visit_paren_ty(inner),
        }
    }

    fn visit_unit_ty(&mut self) {}

    fn visit_lit_ty(&mut self, lit_ty: &LitTy) {}

    fn visit_path_ty(&mut self, path: &PR<Path>) {
        walk_pr!(self, path, visit_path)
    }

    fn visit_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) {
        walk_pr!(self, param_ty, visit_ty);
        walk_pr!(self, return_ty, visit_ty)
    }

    fn visit_paren_ty(&mut self, inner: &PR<N<Ty>>) {
        walk_pr!(self, inner, visit_ty)
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) {}

    fn visit_path(&mut self, path: &Path) {}

    fn visit_block(&mut self, block: &Block) {
        walk_each_pr!(self, block.stmts(), visit_stmt);
        walk_pr!(self, block.expr(), visit_expr)
    }
}
