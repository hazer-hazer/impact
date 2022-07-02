use crate::{
    ast::{
        expr::{InfixOp, Lit, PrefixOp},
        ty::LitTy,
    },
    hir::{
        expr::{Block, Expr},
        item::Item,
        stmt::{Stmt, StmtKind},
        ty::Ty,
        visitor::HirVisitor,
        Path, HIR, N,
    },
    parser::token::Punct,
    span::span::{Ident, Kw},
};

use super::AstLikePP;

macro_rules! walk_block {
    ($self: ident, $nodes: expr, $visitor: ident) => {{
        $self.indent();
        $nodes.iter().for_each(|node| {
            $self.out_indent();
            $self.$visitor(node);
            $self.nl();
        });
        $self.dedent();
    }};
}

macro_rules! walk_each_delim {
    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr) => {
        $nodes.iter().enumerate().for_each(|(index, node)| {
            $self.$visitor(node);
            if index < $nodes.len() - 1 {
                $self.word($sep);
            }
        })
    };
}

impl<'a> HirVisitor for AstLikePP<'a> {
    fn visit_hir(&mut self, hir: &HIR) {
        println!("=== HIR ===");
        walk_each_delim!(self, hir.items(), visit_item, "\n")
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Item(item) => self.visit_item(item),
        }
    }

    // Items //
    fn visit_type_item(&mut self, name: &Ident, ty: &N<Ty>) {
        self.kw(Kw::Type);
        self.visit_ident(name);
        self.punct(Punct::Assign);
        self.visit_ty(ty);
    }

    fn visit_mod_item(&mut self, name: &Ident, items: &Vec<Item>) {
        self.kw(Kw::Mod);
        self.visit_ident(name);
        walk_block!(self, items, visit_item);
    }

    fn visit_decl_item(&mut self, name: &Ident, params: &Vec<Ident>, body: &Expr) {
        self.visit_ident(name);
        self.sp();
        walk_each_delim!(self, params, visit_ident, " ");
        self.punct(Punct::Assign);
        self.visit_expr(body);
    }

    // Expressions //
    fn visit_lit_expr(&mut self, lit: &Lit) {
        self.word(&lit.to_string());
    }

    fn visit_infix_expr(&mut self, lhs: &N<Expr>, op: &InfixOp, rhs: &N<Expr>) {
        self.visit_expr(lhs);
        self.infix(op);
        self.visit_expr(rhs);
    }

    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &N<Expr>) {
        self.prefix(op);
        self.visit_expr(rhs);
    }

    fn visit_abs_expr(&mut self, param: &Ident, body: &N<Expr>) {
        self.punct(Punct::Backslash);
        self.visit_ident(param);
        self.punct(Punct::Arrow);
        self.visit_expr(body);
    }

    fn visit_app_expr(&mut self, lhs: &N<Expr>, arg: &N<Expr>) {
        self.visit_expr(lhs);
        self.sp();
        self.visit_expr(arg);
    }

    fn visit_let_expr(&mut self, block: &Block) {
        self.kw(Kw::Let);
        self.visit_block(block);
    }

    fn visit_type_expr(&mut self, expr: &N<Expr>, ty: &Ty) {
        self.visit_expr(expr);
        self.punct(Punct::Colon);
        self.visit_ty(ty);
    }

    // Types //
    fn visit_unit_ty(&mut self) {
        self.word("()");
    }

    fn visit_lit_ty(&mut self, lit_ty: &LitTy) {
        self.word(&lit_ty.to_string());
    }

    fn visit_func_ty(&mut self, param_ty: &N<Ty>, return_ty: &N<Ty>) {
        self.visit_ty(param_ty);
        self.punct(Punct::Arrow);
        self.visit_ty(return_ty);
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) {
        self.word(&ident.to_string())
    }

    fn visit_path(&mut self, path: &Path) {
        self.word(&path.to_string());
    }

    fn visit_block(&mut self, block: &Block) {
        walk_block!(self, block.stmts(), visit_stmt);
        self.nl();
        self.visit_expr(block.expr());
    }
}
