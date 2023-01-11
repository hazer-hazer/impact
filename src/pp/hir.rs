

use crate::{
    hir::{
        expr::{Block, Call, Infix, Lambda, Lit, Prefix, TyExpr},
        item::{Decl, ItemId, ItemKind, Mod, TypeItem},
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
    pub fn new(sess: &'a Session) -> Self {
        Self {
            pp: AstLikePP::new(sess, AstPPMode::Normal),
        }
    }
}

impl<'a> HirVisitor for HirPP<'a> {
    fn visit_hir(&mut self, hir: &HIR) {
        self.pp.line("== HIR ==");
        walk_each_delim!(self, hir.root().items(), visit_item_stmt, "\n", hir)
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt, hir: &HIR) {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr(expr, hir),
            StmtKind::Item(item) => self.visit_item_stmt(item, hir),
        }
    }

    // Items //
    fn visit_item(&mut self, id: ItemId, hir: &HIR) {
        let item = hir.item(id);
        match item.kind() {
            ItemKind::Type(ty_item) => self.visit_type_item(ty_item, hir),
            ItemKind::Mod(mod_item) => self.visit_mod_item(mod_item, hir),
            ItemKind::Decl(decl) => self.visit_decl_item(decl, hir),
        }
    }

    fn visit_type_item(&mut self, ty_item: &TypeItem, hir: &HIR) {
        self.pp.kw(Kw::Type);
        self.visit_ident(&ty_item.name, hir);
        self.pp.punct(Punct::Assign);
        self.visit_ty(&ty_item.ty, hir);
    }

    fn visit_mod_item(&mut self, mod_item: &Mod, hir: &HIR) {
        self.pp.kw(Kw::Mod);
        self.visit_ident(&mod_item.name, hir);
        self.pp.nl();
        walk_block!(self, mod_item.items, visit_item_stmt, hir);
    }

    fn visit_decl_item(&mut self, decl: &Decl, hir: &HIR) {
        self.visit_ident(&decl.name, hir);
        self.pp.punct(Punct::Assign);
        self.visit_expr(&decl.value, hir);
    }

    // Expressions //
    fn visit_unit_expr(&mut self, _hir: &HIR) {
        self.pp.str("()");
    }

    fn visit_lit_expr(&mut self, lit: &Lit, _hir: &HIR) {
        self.pp.string(lit);
    }

    fn visit_infix_expr(&mut self, infix: &Infix, hir: &HIR) {
        self.visit_expr(&infix.lhs, hir);
        self.pp.infix(&infix.op);
        self.visit_expr(&infix.rhs, hir);
    }

    fn visit_prefix_expr(&mut self, prefix: &Prefix, hir: &HIR) {
        self.pp.prefix(&prefix.op);
        self.visit_expr(&prefix.rhs, hir);
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
    fn visit_unit_ty(&mut self, _hir: &HIR) {
        self.pp.str("()");
    }

    fn visit_func_ty(&mut self, param_ty: &Ty, return_ty: &Ty, hir: &HIR) {
        self.visit_ty(param_ty, hir);
        self.pp.punct(Punct::Arrow);
        self.visit_ty(return_ty, hir);
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident, _hir: &HIR) {
        self.pp.string(ident);
    }

    fn visit_path(&mut self, path: &Path, _hir: &HIR) {
        self.pp.string(path);
    }

    fn visit_block(&mut self, block: &Block, hir: &HIR) {
        self.pp.nl();
        walk_block!(self, block.stmts(), visit_stmt, hir);
        self.pp.indent();
        self.pp.out_indent();
        block.expr().map(|expr| self.visit_expr(expr, hir));
        self.pp.dedent();
    }
}
