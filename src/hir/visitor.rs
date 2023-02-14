use crate::span::span::Ident;

use super::{
    expr::{Block, Call, Expr, ExprKind, Lambda, Lit, PathExpr, TyExpr},
    item::{Decl, ItemId, ItemKind, Mod, TyAlias},
    pat::{Pat, PatKind},
    stmt::{Stmt, StmtKind},
    ty::{Ty, TyKind, TyPath},
    Path, HIR,
};

macro_rules! walk_each {
    ($self: ident, $els: expr, $visitor: ident, $hir: expr) => {
        $els.iter().for_each(|el| {
            $self.$visitor(el, $hir);
        })
    };
}

pub trait HirVisitor {
    fn visit_hir(&mut self, hir: &HIR) {
        walk_each!(self, &hir.root().items, visit_item, hir);
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt, hir: &HIR) {
        let stmt = hir.stmt(*stmt);
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(&expr, hir),
            StmtKind::Item(item) => self.visit_item_stmt(&item, hir),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &Expr, hir: &HIR) {
        self.visit_expr(expr, hir)
    }

    fn visit_item_stmt(&mut self, id: &ItemId, hir: &HIR) {
        self.visit_item(id, hir)
    }

    // Items //
    fn visit_item(&mut self, id: &ItemId, hir: &HIR) {
        let id = *id;
        let item = hir.item(id);
        match item.kind() {
            ItemKind::TyAlias(ty) => self.visit_type_item(item.name(), ty, id, hir),
            ItemKind::Mod(m) => self.visit_mod_item(item.name(), m, id, hir),
            ItemKind::Decl(decl) => self.visit_decl_item(item.name(), decl, id, hir),
        }
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, _id: ItemId, hir: &HIR) {
        self.visit_ident(&name, hir);
        self.visit_ty(&ty_item.ty, hir);
    }

    fn visit_mod_item(&mut self, name: Ident, mod_item: &Mod, _id: ItemId, hir: &HIR) {
        self.visit_ident(&name, hir);
        walk_each!(self, mod_item.items, visit_item, hir);
    }

    fn visit_decl_item(&mut self, name: Ident, decl: &Decl, _id: ItemId, hir: &HIR) {
        self.visit_ident(&name, hir);
        self.visit_expr(&decl.value, hir);
    }

    // Patterns //
    fn visit_pat(&mut self, pat: &Pat, hir: &HIR) {
        let pat = hir.pat(*pat);
        match pat.kind() {
            PatKind::Unit => self.visit_unit_pat(),
            PatKind::Ident(ident) => self.visit_ident_pat(&ident, hir),
        }
    }

    fn visit_unit_pat(&mut self) {}

    fn visit_ident_pat(&mut self, ident: &Ident, hir: &HIR) {
        self.visit_ident(ident, hir);
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr, hir: &HIR) {
        let expr = hir.expr(*expr);
        match &expr.kind() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit, hir),
            ExprKind::Path(path) => self.visit_path_expr(path, hir),
            ExprKind::Block(block) => self.visit_block_expr(block, hir),
            ExprKind::Call(call) => self.visit_call_expr(call, hir),
            ExprKind::Let(block) => self.visit_let_expr(block, hir),
            ExprKind::Lambda(lambda) => self.visit_lambda(lambda, hir),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr, hir),
        }
    }

    fn visit_lit_expr(&mut self, _: &Lit, _hir: &HIR) {}

    fn visit_path_expr(&mut self, path: &PathExpr, hir: &HIR) {
        self.visit_path(&path.0, hir)
    }

    fn visit_block_expr(&mut self, block: &Block, hir: &HIR) {
        self.visit_block(block, hir)
    }

    fn visit_call_expr(&mut self, call: &Call, hir: &HIR) {
        self.visit_expr(&call.lhs, hir);
        self.visit_expr(&call.arg, hir);
    }

    fn visit_lambda(&mut self, lambda: &Lambda, hir: &HIR) {
        self.visit_pat(&lambda.param, hir);
        self.visit_expr(&lambda.body, hir);
    }

    fn visit_let_expr(&mut self, block: &Block, hir: &HIR) {
        self.visit_block(block, hir);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr, hir: &HIR) {
        self.visit_expr(&ty_expr.expr, hir);
        self.visit_ty(&ty_expr.ty, hir);
    }

    // Types //
    fn visit_ty(&mut self, ty: &Ty, hir: &HIR) {
        let ty = hir.ty(*ty);
        match &ty.kind {
            TyKind::Path(path) => self.visit_ty_path(path, hir),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty, hir),
        }
    }

    fn visit_ty_path(&mut self, path: &TyPath, hir: &HIR) {
        self.visit_path(&path.0, hir)
    }

    fn visit_func_ty(&mut self, param_ty: &Ty, return_ty: &Ty, hir: &HIR) {
        self.visit_ty(param_ty, hir);
        self.visit_ty(return_ty, hir);
    }

    // Fragments //
    fn visit_ident(&mut self, _: &Ident, _hir: &HIR) {}

    fn visit_path(&mut self, _: &Path, _hir: &HIR) {}

    fn visit_block(&mut self, block: &Block, hir: &HIR) {
        let block = hir.block(*block);
        walk_each!(self, block.stmts(), visit_stmt, hir);

        block.expr().map(|expr| self.visit_expr(expr, hir));
    }
}
