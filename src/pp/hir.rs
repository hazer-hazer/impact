use crate::{
    ast::{
        expr::{InfixOp, Lit, PrefixOp},
        ty::LitTy,
    },
    hir::{
        expr::Expr,
        item::Item,
        stmt::{Stmt, StmtKind},
        ty::Ty,
        visitor::HirVisitor,
        Path, HIR, N,
    },
    span::span::Ident,
};

use super::AstLikePP;

macro_rules! visit_block {
    ($self: ident, $nodes: expr, $visitor: ident) => {{
        $self.indent();
        let string = $nodes
            .iter()
            .map(|node| format!("{}{}", $self.cur_indent(), $self.$visitor(node)))
            .collect::<Vec<_>>()
            .join("\n");
        $self.dedent();
        string
    }};
}

macro_rules! visit_each {
    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr) => {
        $nodes
            .iter()
            .map(|node| $self.$visitor(node))
            .collect::<Vec<_>>()
            .join($sep)
    };
}

impl<'a> HirVisitor<String> for AstLikePP<'a> {
    fn visit_hir(&mut self, hir: &HIR) -> String {
        println!("=== HIR ===");
        visit_each!(self, hir.items(), visit_item, "\n")
    }

    // Statements //
    fn visit_stmt(&mut self, stmt: &Stmt) -> String {
        format!(
            "{}{}",
            self.cur_indent(),
            match stmt.kind() {
                StmtKind::Expr(expr) => self.visit_expr(expr),
                StmtKind::Item(item) => self.visit_item(item),
            }
        )
    }

    // Items //
    fn visit_type_item(&mut self, name: &Ident, ty: &N<Ty>) -> String {
        format!("type {} = {}", self.visit_ident(name), self.visit_ty(ty))
    }

    fn visit_mod_item(&mut self, name: &Ident, items: &Vec<Item>) -> String {
        format!("mod {}\n{}", name, visit_block!(self, items, visit_item))
    }

    fn visit_decl_item(&mut self, name: &Ident, params: &Vec<Ident>, body: &Expr) -> String {
        format!(
            "{} {} = {}",
            self.visit_ident(name),
            visit_each!(self, params, visit_ident, " "),
            self.visit_expr(body)
        )
    }

    // Expressions //
    fn visit_lit_expr(&mut self, lit: &Lit) -> String {
        lit.to_string()
    }

    fn visit_infix_expr(&mut self, lhs: &N<Expr>, op: &InfixOp, rhs: &N<Expr>) -> String {
        format!("{} {} {}", self.visit_expr(lhs), op, self.visit_expr(rhs))
    }

    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &N<Expr>) -> String {
        format!("{}{}", op, self.visit_expr(rhs))
    }

    fn visit_abs_expr(&mut self, param: &Ident, body: &N<Expr>) -> String {
        format!("\\{} -> {}", self.visit_ident(param), self.visit_expr(body))
    }

    fn visit_app_expr(&mut self, lhs: &N<Expr>, arg: &N<Expr>) -> String {
        format!("{} {}", self.visit_expr(lhs), self.visit_expr(arg))
    }

    fn visit_block_expr(&mut self, stmts: &Vec<Stmt>) -> String {
        visit_block!(self, stmts, visit_stmt)
    }

    fn visit_let_expr(&mut self, name: &Ident, value: &N<Expr>, body: &N<Expr>) -> String {
        format!(
            "let {} = {} in {}",
            self.visit_ident(name),
            self.visit_expr(value),
            self.visit_expr(body)
        )
    }

    fn visit_type_expr(&mut self, expr: &N<Expr>, ty: &Ty) -> String {
        format!("{}: {}", self.visit_expr(expr), self.visit_ty(ty))
    }

    // Types //
    fn visit_unit_ty(&mut self) -> String {
        "()".to_string()
    }

    fn visit_lit_ty(&mut self, lit_ty: &LitTy) -> String {
        format!("{}", lit_ty)
    }

    fn visit_func_ty(&mut self, param_ty: &N<Ty>, return_ty: &N<Ty>) -> String {
        format!(
            "{} -> {}",
            self.visit_ty(param_ty),
            self.visit_ty(return_ty)
        )
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) -> String {
        ident.to_string()
    }

    fn visit_path(&mut self, path: &Path) -> String {
        format!(
            "{}",
            path.segments()
                .iter()
                .map(|seg| format!("{}", seg))
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}
