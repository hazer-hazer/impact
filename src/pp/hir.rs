use crate::{
    hir::{
        expr::{Expr, ExprKind},
        stmt::{Stmt, StmtKind},
        visitor::Visitor,
        HIR,
    },
    ast::expr::Lit,
    span::span::Ident,
};

use super::{match_kind, AstLikePP, PP};

macro_rules! visit_each {
    ($self: ident, $nodes: expr, $visitor: ident, $sep: expr) => {
        $nodes
            .iter()
            .map(|node| $self.$visitor(node))
            .collect::<Vec<_>>()
            .join($sep)
    };
}

impl<'a> Visitor<'a, String> for AstLikePP<'a> {
    fn visit_hir(&mut self, hir: &'a HIR) -> String {
        visit_each!(self, hir.stmts(), visit_stmt, "\n")
    }

    fn visit_expr(&mut self, expr: &'a Expr) -> String {
        match expr.node() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Ident(ident) => self.visit_ident_expr(ident),
            expr @ ExprKind::Infix(_, _, _) => self.visit_infix_expr(expr),
            expr @ ExprKind::Prefix(_, _) => self.visit_prefix_expr(expr),
            expr @ ExprKind::Abs(_, _) => self.visit_abs_expr(expr),
            expr @ ExprKind::App(_, _) => self.visit_app_expr(expr),
            expr @ ExprKind::Block(_) => self.visit_block_expr(expr),
            expr @ ExprKind::Let(_, _, _) => self.visit_let_expr(expr),
        }
    }

    fn visit_lit_expr(&mut self, lit: &Lit) -> String {
        lit.ppfmt(self.sess)
    }

    fn visit_ident_expr(&mut self, ident: &Ident) -> String {
        ident.ppfmt(self.sess)
    }

    fn visit_infix_expr(&mut self, expr: &'a ExprKind<'a>) -> String {
        match_kind!(expr, ExprKind::Infix(lhs, op, rhs), {
            format!(
                "{} {} {}",
                self.visit_expr(lhs),
                op.ppfmt(self.sess),
                self.visit_expr(rhs)
            )
        })
    }

    fn visit_prefix_expr(&mut self, expr: &'a ExprKind<'a>) -> String {
        match_kind!(expr, ExprKind::Prefix(op, rhs), {
            format!("{}{}", op.ppfmt(self.sess), self.visit_expr(rhs))
        })
    }

    fn visit_abs_expr(&mut self, expr: &'a ExprKind<'a>) -> String {
        match_kind!(expr, ExprKind::Abs(param, body), {
            format!("\\{} -> {}", self.visit_ident(param), self.visit_expr(body))
        })
    }

    fn visit_app_expr(&mut self, expr: &'a ExprKind<'a>) -> String {
        match_kind!(expr, ExprKind::App(lhs, arg), {
            format!("{} {}", self.visit_expr(lhs), self.visit_expr(arg))
        })
    }

    fn visit_block_expr(&mut self, expr: &'a ExprKind<'a>) -> String {
        match_kind!(expr, ExprKind::Block(stmts), {
            self.indent();
            let output = visit_each!(self, stmts, visit_stmt, "\n");
            self.dedent();
            output
        })
    }

    fn visit_let_expr(&mut self, expr: &'a ExprKind<'a>) -> String {
        match_kind!(expr, ExprKind::Let(name, value, body), {
            format!(
                "let {} = {} in {}",
                self.visit_ident(name),
                self.visit_expr(value),
                self.visit_expr(body)
            )
        })
    }

    fn visit_stmt(&mut self, stmt: &'a Stmt<'a>) -> String {
        format!(
            "{}{}",
            self.cur_indent(),
            match stmt.node() {
                StmtKind::Expr(expr) => self.visit_expr(expr),
            }
        )
    }

    fn visit_ident(&mut self, ident: &Ident) -> String {
        ident.ppfmt(self.sess)
    }
}
