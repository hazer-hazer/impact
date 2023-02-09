use crate::{
    hir::{
        expr::{Block, Call, Expr, ExprKind, Lambda, Lit, TyExpr},
        item::{Decl, ItemId, ItemKind, Mod, TyAlias},
        pat::{Pat, PatKind},
        stmt::{Stmt, StmtKind},
        ty::{Ty, TyKind},
        visitor::HirVisitor,
        Path, HIR,
    },
    parser::token::Punct,
    session::Session,
    span::span::{Ident, Kw},
};

use super::{AstLikePP, AstPPMode};

macro_rules! walk_block {
    ($self: ident, $nodes: expr, $visitor: ident, $hir: expr) => {{
        $self.pp.indent();
        $nodes.iter().for_each(|node| {
            $self.pp.out_indent();
            $self.$visitor(node, $hir);
            $self.pp.nl();
        });
        $self.pp.dedent();
    }};
}

macro_rules! walk_each_delim {
    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr, $hir: expr) => {
        $nodes.iter().enumerate().for_each(|(index, node)| {
            $self.$visitor(node, $hir);
            if index < $nodes.len() - 1 {
                $self.pp.str($sep);
            }
        })
    };
}

pub struct HirPP<'a> {
    pub pp: AstLikePP<'a>,
}

impl<'a> HirPP<'a> {
    pub fn new(sess: &'a Session, mode: AstPPMode) -> Self {
        Self {
            pp: AstLikePP::new(sess, mode),
        }
    }
}

impl<'a> HirVisitor for HirPP<'a> {
    fn visit_hir(&mut self, hir: &HIR) {
        self.pp.line("== HIR ==");
        walk_each_delim!(self, hir.root().items, visit_item_stmt, "\n", hir)
    }

    fn visit_stmt(&mut self, stmt: &Stmt, hir: &HIR) {
        let stmt = hir.stmt(*stmt);
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr_stmt(&expr, hir),
            StmtKind::Item(item) => self.visit_item_stmt(&item, hir),
        }
        self.pp.hir_id(stmt);
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

        if self.pp.sess.config().pp_ast_ids() {
            self.pp.string(id);
        }
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, _id: ItemId, hir: &HIR) {
        self.pp.kw(Kw::Type);
        self.visit_ident(&name, hir);
        self.pp.str(" = ");
        self.visit_ty(&ty_item.ty, hir);
    }

    fn visit_mod_item(&mut self, name: Ident, mod_item: &Mod, _id: ItemId, hir: &HIR) {
        self.pp.kw(Kw::Mod);
        self.visit_ident(&name, hir);
        self.pp.nl();
        walk_block!(self, mod_item.items, visit_item, hir);
    }

    fn visit_decl_item(&mut self, name: Ident, decl: &Decl, id: ItemId, hir: &HIR) {
        self.visit_ident(&name, hir);
        self.pp.ty_anno(id.hir_id());
        self.pp.str(" = ");
        self.visit_expr(&decl.value, hir);
    }

    // Expressions //
    fn visit_expr(&mut self, expr_id: &Expr, hir: &HIR) {
        let expr_id = *expr_id;
        let expr = hir.expr(expr_id);
        match &expr.kind() {
            ExprKind::Unit => self.visit_unit_expr(hir),
            ExprKind::Lit(lit) => self.visit_lit_expr(lit, hir),
            ExprKind::Path(path) => self.visit_path_expr(path, hir),
            ExprKind::Block(block) => self.visit_block_expr(block, hir),
            ExprKind::Call(call) => self.visit_call_expr(call, hir),
            ExprKind::Let(block) => self.visit_let_expr(block, hir),
            ExprKind::Lambda(lambda) => self.visit_lambda(lambda, hir),
            ExprKind::Ty(ty_expr) => self.visit_type_expr(ty_expr, hir),
        }
        self.pp.ty_anno(expr_id);
        self.pp.hir_id(expr);
    }

    fn visit_unit_expr(&mut self, _hir: &HIR) {
        self.pp.str("()");
    }

    fn visit_lit_expr(&mut self, lit: &Lit, _hir: &HIR) {
        self.pp.string(lit);
    }

    fn visit_lambda(&mut self, lambda: &Lambda, hir: &HIR) {
        self.pp.punct(Punct::Backslash);
        self.visit_pat(&lambda.param, hir);
        self.pp.punct(Punct::Arrow);
        self.visit_expr(&lambda.body, hir);
    }

    fn visit_call_expr(&mut self, call: &Call, hir: &HIR) {
        self.visit_expr(&call.lhs, hir);
        self.pp.sp();
        self.visit_expr(&call.arg, hir);
    }

    fn visit_let_expr(&mut self, block: &Block, hir: &HIR) {
        self.pp.kw(Kw::Let);
        self.visit_block(block, hir);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr, hir: &HIR) {
        self.visit_expr(&ty_expr.expr, hir);
        self.pp.punct(Punct::Colon);
        self.visit_ty(&ty_expr.ty, hir);
    }

    // Types //
    fn visit_ty(&mut self, ty: &Ty, hir: &HIR) {
        let ty = hir.ty(*ty);
        match &ty.kind {
            TyKind::Unit => self.visit_unit_ty(hir),
            TyKind::Path(path) => self.visit_path_ty(path, hir),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty, hir),
        }
        self.pp.hir_id(ty);
    }

    fn visit_unit_ty(&mut self, _hir: &HIR) {
        self.pp.str("()");
    }

    fn visit_func_ty(&mut self, param_ty: &Ty, return_ty: &Ty, hir: &HIR) {
        self.visit_ty(param_ty, hir);
        self.pp.punct(Punct::Arrow);
        self.visit_ty(return_ty, hir);
    }

    // Patterns //
    fn visit_pat(&mut self, pat: &Pat, hir: &HIR) {
        let pat = hir.pat(*pat);
        match pat.kind() {
            PatKind::Ident(ident) => self.visit_ident_pat(&ident, hir),
        }
        self.pp.hir_id(pat);
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident, _hir: &HIR) {
        self.pp.string(ident);
    }

    fn visit_path(&mut self, path: &Path, hir: &HIR) {
        let path = hir.path(*path);
        self.pp.string(path);
        self.pp.hir_id(path);
    }

    fn visit_block(&mut self, block: &Block, hir: &HIR) {
        let block = hir.block(*block);

        self.pp.nl();
        walk_block!(self, block.stmts(), visit_stmt, hir);
        self.pp.indent();
        self.pp.out_indent();
        block.expr().map(|expr| self.visit_expr(expr, hir));
        self.pp.dedent();
        self.pp.hir_id(block);
    }
}
