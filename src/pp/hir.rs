use crate::{
    hir::{
        expr::{Block, Call, Infix, Lambda, Lit, Prefix, TyExpr},
        item::{Decl, Mod, TypeItem},
        stmt::{Stmt, StmtKind},
        ty::Ty,
        visitor::HirVisitor,
        Path, HIR,
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
                $self.str($sep);
            }
        })
    };
}

impl<'a> HirVisitor for AstLikePP<'a> {
    fn visit_hir(&mut self, hir: &HIR) {
        self.line("== HIR ==");
        // walk_each_delim!(self, hir.items(), visit_item, "\n")
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Item(item) => self.visit_item(item),
        }
    }

    // Items //
    fn visit_type_item(&mut self, ty_item: &TypeItem) {
        self.kw(Kw::Type);
        self.visit_ident(&ty_item.name);
        self.punct(Punct::Assign);
        self.visit_ty(&ty_item.ty);
    }

    fn visit_mod_item(&mut self, mod_item: &Mod) {
        self.kw(Kw::Mod);
        self.visit_ident(&mod_item.name);
        self.nl();
        walk_block!(self, &mod_item.items, visit_item);
    }

    fn visit_decl_item(&mut self, decl: &Decl) {
        self.visit_ident(&decl.name);
        self.punct(Punct::Assign);
        self.visit_expr(&decl.value);
    }

    // Expressions //
    fn visit_unit_expr(&mut self) {
        self.str("()");
    }

    fn visit_lit_expr(&mut self, lit: &Lit) {
        self.string(lit);
    }

    fn visit_infix_expr(&mut self, infix: &Infix) {
        self.visit_expr(&infix.lhs);
        self.infix(&infix.op);
        self.visit_expr(&infix.rhs);
    }

    fn visit_prefix_expr(&mut self, prefix: &Prefix) {
        self.prefix(&prefix.op);
        self.visit_expr(&prefix.rhs);
    }

    fn visit_lambda(&mut self, lambda: &Lambda) {
        self.punct(Punct::Backslash);
        self.visit_pat(&lambda.param);
        self.punct(Punct::Arrow);
        self.visit_expr(&lambda.body);
    }

    fn visit_call_expr(&mut self, call: &Call) {
        self.visit_expr(&call.lhs);
        self.sp();
        self.visit_expr(&call.arg);
    }

    fn visit_let_expr(&mut self, block: &Block) {
        self.kw(Kw::Let);
        self.visit_block(block);
    }

    fn visit_type_expr(&mut self, ty_expr: &TyExpr) {
        self.visit_expr(&ty_expr.expr);
        self.punct(Punct::Colon);
        self.visit_ty(&ty_expr.ty);
    }

    // Types //
    fn visit_unit_ty(&mut self) {
        self.str("()");
    }

    fn visit_func_ty(&mut self, param_ty: &Ty, return_ty: &Ty) {
        self.visit_ty(param_ty);
        self.punct(Punct::Arrow);
        self.visit_ty(return_ty);
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) {
        self.string(ident);
    }

    fn visit_path(&mut self, path: &Path) {
        self.string(path);
    }

    fn visit_block(&mut self, block: &Block) {
        self.nl();
        walk_block!(self, block.stmts(), visit_stmt);
        self.indent();
        self.out_indent();
        block.expr().map(|expr| self.visit_expr(expr));
        self.dedent();
    }
}
