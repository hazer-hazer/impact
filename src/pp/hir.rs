use crate::{
    hir::{
        expr::{Block, Call, Infix, Lambda, Lit, Prefix, TyExpr},
        item::{Decl, ItemId, Mod, TypeItem, ItemKind},
        stmt::{Stmt, StmtKind},
        ty::Ty,
        visitor::HirVisitor,
        Path, HIR,
    },
    parser::token::Punct,
    session::Session,
    span::span::{Ident, Kw},
};

use super::{AstLikePP, AstPPMode};

macro_rules! walk_block {
    ($self: ident, $nodes: expr, $visitor: ident) => {{
        $self.pp.indent();
        $nodes.iter().for_each(|node| {
            $self.pp.out_indent();
            $self.$visitor(node);
            $self.pp.nl();
        });
        $self.pp.dedent();
    }};
}

macro_rules! walk_each_delim {
    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr) => {
        $nodes.iter().enumerate().for_each(|(index, node)| {
            $self.$visitor(node);
            if index < $nodes.len() - 1 {
                $self.pp.str($sep);
            }
        })
    };
}

pub struct HirPP<'a> {
    pub pp: AstLikePP<'a>,
    hir: &'a HIR,
}

impl<'a> HirPP<'a> {
    pub fn new(sess: &'a Session, hir: &'a HIR) -> Self {
        Self {
            pp: AstLikePP::new(sess, AstPPMode::Normal),
            hir,
        }
    }
}

impl<'a> HirVisitor for HirPP<'a> {
    fn hir(&self) -> &HIR {
        &self.hir
    }

    fn visit_hir(&mut self) {
        self.pp.line("== HIR ==");
        walk_each_delim!(self, self.hir().root().items(), visit_item_stmt, "\n")
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Item(item) => self.visit_item_stmt(item),
        }
    }

    // Items //
    fn visit_item(&mut self, id: ItemId) {
        let item = self.hir().item(id);
        match item.kind() {
            ItemKind::Type(ty_item) => self.visit_type_item(ty_item),
            ItemKind::Mod(mod_item) => self.visit_mod_item(mod_item),
            ItemKind::Decl(decl) => self.visit_decl_item(decl),
        }
    }

    fn visit_type_item(&mut self, ty_item: &TypeItem) {
        self.pp.kw(Kw::Type);
        self.visit_ident(&ty_item.name);
        self.pp.punct(Punct::Assign);
        self.visit_ty(&ty_item.ty);
    }

    fn visit_mod_item(&mut self, mod_item: &Mod) {
        self.pp.kw(Kw::Mod);
        self.visit_ident(&mod_item.name);
        self.pp.nl();
        walk_block!(self, mod_item.items, visit_item_stmt);
    }

    fn visit_decl_item(&mut self, decl: &Decl) {
        self.visit_ident(&decl.name);
        self.pp.punct(Punct::Assign);
        self.visit_expr(&decl.value);
    }

    // Expressions //
    fn visit_unit_expr(&mut self) {
        self.pp.str("()");
    }

    fn visit_lit_expr(&mut self, lit: &Lit) {
        self.pp.string(lit);
    }

    fn visit_infix_expr(&mut self, infix: &Infix) {
        self.visit_expr(&infix.lhs);
        self.pp.infix(&infix.op);
        self.visit_expr(&infix.rhs);
    }

    fn visit_prefix_expr(&mut self, prefix: &Prefix) {
        self.pp.prefix(&prefix.op);
        self.visit_expr(&prefix.rhs);
    }

    fn visit_lambda(&mut self, lambda: &Lambda) {
        self.pp.punct(Punct::Backslash);
        self.visit_pat(&lambda.param);
        self.pp.punct(Punct::Arrow);
        self.visit_expr(&lambda.body);
    }

    fn visit_call_expr(&mut self, call: &Call) {
        self.visit_expr(&call.lhs);
        self.pp.sp();
        self.visit_expr(&call.arg);
    }

    fn visit_let_expr(&mut self, block: &Block) {
        self.pp.kw(Kw::Let);
        self.visit_block(block);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr) {
        self.visit_expr(&ty_expr.expr);
        self.pp.punct(Punct::Colon);
        self.visit_ty(&ty_expr.ty);
    }

    // Types //
    fn visit_unit_ty(&mut self) {
        self.pp.str("()");
    }

    fn visit_func_ty(&mut self, param_ty: &Ty, return_ty: &Ty) {
        self.visit_ty(param_ty);
        self.pp.punct(Punct::Arrow);
        self.visit_ty(return_ty);
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) {
        self.pp.string(ident);
    }

    fn visit_path(&mut self, path: &Path) {
        self.pp.string(path);
    }

    fn visit_block(&mut self, block: &Block) {
        self.pp.nl();
        walk_block!(self, block.stmts(), visit_stmt);
        self.pp.indent();
        self.pp.out_indent();
        block.expr().map(|expr| self.visit_expr(expr));
        self.pp.dedent();
    }
}
