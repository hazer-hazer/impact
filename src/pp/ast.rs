use crate::{
    ast::{
        expr::{Block, Expr, ExprKind, InfixOp, Lit, PrefixOp},
        item::Item,
        stmt::{Stmt, StmtKind},
        ty::{LitTy, Ty},
        visitor::walk_pr,
        visitor::AstVisitor,
        ErrorNode, Path, AST, N, PR,
    },
    parser::token::Punct,
    span::span::{Ident, Kw},
};

use super::AstLikePP;

macro_rules! walk_block {
    ($self: ident, $prs: expr, $ok_visitor: ident) => {{
        $self.indent();
        $prs.iter().for_each(|pr| {
            $self.out_indent();
            walk_pr!($self, pr, $ok_visitor);
            $self.nl();
        });
        $self.dedent();
    }};
}

// Would be nice if intersperse was stable
macro_rules! walk_each_pr_delim {
    ($self: ident, $prs: expr, $ok_visitor: ident, $sep: expr) => {
        $prs.iter().enumerate().for_each(|(index, pr)| {
            walk_pr!($self, pr, $ok_visitor);
            if index < $prs.len() - 1 {
                $self.str($sep);
            }
        })
    };
}

impl<'a> AstVisitor for AstLikePP<'a> {
    fn visit_err(&mut self, _: &ErrorNode) {
        self.out.push_str("[ERROR]")
    }

    fn visit_ast(&mut self, ast: &AST) {
        println!("=== AST ===");
        walk_each_pr_delim!(self, ast.items(), visit_item, "\n")
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt.kind() {
            StmtKind::Expr(expr) => walk_pr!(self, expr, visit_expr),
            StmtKind::Item(item) => walk_pr!(self, item, visit_item),
        }
    }

    // Items //
    fn visit_type_item(&mut self, name: &PR<Ident>, ty: &PR<N<Ty>>) {
        self.kw(Kw::Type);
        walk_pr!(self, name, visit_ident);
        self.punct(Punct::Assign);
        walk_pr!(self, ty, visit_ty);
    }

    fn visit_mod_item(&mut self, name: &PR<Ident>, items: &Vec<PR<N<Item>>>) {
        self.kw(Kw::Mod);
        walk_pr!(self, name, visit_ident);
        self.nl();
        walk_block!(self, items, visit_item);
    }

    fn visit_decl_item(&mut self, name: &PR<Ident>, params: &Vec<PR<Ident>>, body: &PR<N<Expr>>) {
        walk_pr!(self, name, visit_ident);
        if !params.is_empty() {
            self.sp();
        }
        walk_each_pr_delim!(self, params, visit_ident, " ");
        self.punct(Punct::Assign);
        walk_pr!(self, body, visit_expr);
    }

    // Expressions //
    fn visit_lit_expr(&mut self, lit: &Lit) {
        self.string(lit);
    }

    fn visit_infix_expr(&mut self, lhs: &PR<N<Expr>>, op: &InfixOp, rhs: &PR<N<Expr>>) {
        walk_pr!(self, lhs, visit_expr);
        self.infix(op);
        walk_pr!(self, rhs, visit_expr);
    }

    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) {
        self.prefix(op);
        walk_pr!(self, rhs, visit_expr);
    }

    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) {
        self.punct(Punct::Backslash);
        walk_pr!(self, param, visit_ident);
        self.punct(Punct::Arrow);
        walk_pr!(self, body, visit_expr);
    }

    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) {
        walk_pr!(self, lhs, visit_expr);
        self.sp();
        walk_pr!(self, arg, visit_expr);
    }

    fn visit_let_expr(&mut self, block: &PR<Block>) {
        self.kw(Kw::Let);
        walk_pr!(self, block, visit_block);
    }

    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) {
        walk_pr!(self, expr, visit_expr);
        self.punct(Punct::Colon);
        walk_pr!(self, ty, visit_ty);
    }

    // Types //
    fn visit_unit_ty(&mut self) {
        self.str("()");
    }

    fn visit_lit_ty(&mut self, lit_ty: &LitTy) {
        self.string(lit_ty);
    }

    fn visit_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) {
        walk_pr!(self, param_ty, visit_ty);
        self.punct(Punct::Arrow);
        walk_pr!(self, return_ty, visit_ty);
    }

    fn visit_paren_ty(&mut self, inner: &PR<N<Ty>>) {
        self.punct(Punct::LParen);
        walk_pr!(self, inner, visit_ty);
        self.punct(Punct::RParen);
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
    }
}
