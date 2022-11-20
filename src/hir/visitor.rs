use crate::span::span::Ident;

use super::{
    expr::{Block, Expr, ExprKind, FuncCall, Infix, Lambda, Lit, PathExpr, Prefix, TyExpr},
    item::{Decl, Item, ItemKind, Mod, TypeItem},
    stmt::{Stmt, StmtKind},
    ty::{Ty, TyKind},
    Path, HIR, N,
};

macro_rules! walk_each {
    ($self: ident, $els: expr, $visitor: ident) => {
        for el in $els {
            $self.$visitor(el);
        }
    };
}

pub trait HirVisitor {
    fn visit_hir(&mut self, hir: &HIR) {
        walk_each!(self, hir.items(), visit_item);
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) {
        self.visit_expr(expr)
    }

    fn visit_item_stmt(&mut self, item: &Item) {
        self.visit_item(item)
    }

    // Items //
    fn visit_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::Type(ty_item) => self.visit_type_item(ty_item),
            ItemKind::Mod(mod_item) => self.visit_mod_item(mod_item),
            ItemKind::Decl(decl) => self.visit_decl_item(decl),
        }
    }

    fn visit_type_item(&mut self, ty_item: &TypeItem) {
        self.visit_ident(&ty_item.name);
        self.visit_ty(&ty_item.ty);
    }

    fn visit_mod_item(&mut self, mod_item: &Mod) {
        self.visit_ident(&mod_item.name);
        walk_each!(self, &mod_item.items, visit_item);
    }

    fn visit_decl_item(&mut self, decl: &Decl) {
        self.visit_ident(&decl.name);
        walk_each!(self, &decl.params, visit_ident);
        self.visit_expr(&decl.body);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) {
        match expr.kind() {
            ExprKind::Unit => self.visit_unit_expr(),
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Path(path) => self.visit_path_expr(path),
            ExprKind::Block(block) => self.visit_block_expr(block),
            ExprKind::Infix(infix) => self.visit_infix_expr(infix),
            ExprKind::Prefix(prefix) => self.visit_prefix_expr(prefix),
            ExprKind::Call(call) => self.visit_call_expr(call),
            ExprKind::Let(block) => self.visit_let_expr(block),
            ExprKind::Lambda(lambda) => self.visit_lambda(lambda),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr),
        }
    }

    fn visit_unit_expr(&mut self) {}

    fn visit_lit_expr(&mut self, _: &Lit) {}

    fn visit_path_expr(&mut self, path: &PathExpr) {
        self.visit_path(&path.0)
    }

    fn visit_block_expr(&mut self, block: &Block) {
        self.visit_block(block)
    }

    fn visit_infix_expr(&mut self, infix: &Infix) {
        self.visit_expr(&infix.lhs);
        self.visit_expr(&infix.rhs);
    }

    fn visit_prefix_expr(&mut self, prefix: &Prefix) {
        self.visit_expr(&prefix.rhs);
    }

    fn visit_call_expr(&mut self, call: &FuncCall) {
        self.visit_expr(&call.lhs);
        self.visit_expr(&call.arg);
    }

    fn visit_lambda(&mut self, lambda: &Lambda) {
        self.visit_ident(&lambda.param);
        self.visit_expr(&lambda.body);
    }

    fn visit_let_expr(&mut self, block: &Block) {
        self.visit_block(block);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr) {
        self.visit_expr(&ty_expr.expr);
        self.visit_ty(&ty_expr.ty);
    }

    // Types //
    fn visit_ty(&mut self, ty: &Ty) {
        match ty.kind() {
            TyKind::Unit => self.visit_unit_ty(),
            TyKind::Path(path) => self.visit_path_ty(path),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
        }
    }

    fn visit_unit_ty(&mut self) {}

    fn visit_path_ty(&mut self, path: &Path) {
        self.visit_path(path)
    }

    fn visit_func_ty(&mut self, param_ty: &N<Ty>, return_ty: &N<Ty>) {
        self.visit_ty(param_ty);
        self.visit_ty(return_ty);
    }

    // Fragments //
    fn visit_ident(&mut self, _: &Ident) {}

    fn visit_path(&mut self, _: &Path) {}

    fn visit_block(&mut self, block: &Block) {
        walk_each!(self, block.stmts(), visit_stmt);

        block.expr().map(|expr| self.visit_expr(expr));
    }
}
