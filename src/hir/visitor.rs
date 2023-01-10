use crate::span::span::Ident;

use super::{
    expr::{Block, Call, Expr, ExprKind, Infix, Lambda, Lit, PathExpr, Prefix, TyExpr},
    item::{Decl, Item, ItemId, ItemKind, Mod, TypeItem},
    pat::{Pat, PatKind},
    stmt::{Stmt, StmtKind},
    ty::{Ty, TyKind},
    Path, HIR,
};

macro_rules! walk_each {
    ($self: ident, $els: expr, $visitor: ident) => {
        for el in $els {
            $self.$visitor(el);
        }
    };
}

pub trait HirVisitor {
    fn hir(&self) -> &HIR;

    fn visit_hir(&mut self) {
        walk_each!(self, &self.hir().root.items, visit_item_stmt);
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

    fn visit_item_stmt(&mut self, id: &ItemId) {
        self.visit_item(*id)
    }

    // Items //
    fn visit_item(&mut self, id: ItemId);

    fn visit_type_item(&mut self, ty_item: &TypeItem) {
        self.visit_ident(&ty_item.name);
        self.visit_ty(&ty_item.ty);
    }

    fn visit_mod_item(&mut self, mod_item: &Mod) {
        self.visit_ident(&mod_item.name);
        walk_each!(self, &mod_item.items, visit_item_stmt);
    }

    fn visit_decl_item(&mut self, decl: &Decl) {
        self.visit_ident(&decl.name);
        self.visit_expr(&decl.value);
    }

    // Patterns //
    fn visit_pat(&mut self, pat: &Pat) {
        match pat.kind() {
            PatKind::Ident(ident) => self.visit_ident_pat(ident),
        }
    }

    fn visit_ident_pat(&mut self, ident: &Ident) {
        self.visit_ident(ident);
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

    fn visit_call_expr(&mut self, call: &Call) {
        self.visit_expr(&call.lhs);
        self.visit_expr(&call.arg);
    }

    fn visit_lambda(&mut self, lambda: &Lambda) {
        self.visit_pat(&lambda.param);
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

    fn visit_func_ty(&mut self, param_ty: &Ty, return_ty: &Ty) {
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
