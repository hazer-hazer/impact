use crate::{
    ast::{
        expr::{Expr, ExprKind, Lit},
        stmt::{Stmt, StmtKind},
        visitor::Visitor,
        ErrorNode, AST,
    },
    pp::PP,
    session::Session,
    span::span::Ident,
};

use super::AstLikePP;
use super::match_kind;

macro_rules! visit_pr {
    ($self: ident, $pr: expr, $ok_visitor: ident) => {
        match $pr {
            Ok(ok) => $self.$ok_visitor(ok),
            Err(err) => $self.visit_err(err),
        }
    };
}

macro_rules! visit_pr_vec {
    ($self: ident, $prs: expr, $ok_visitor: ident, $sep: expr) => {
        $prs.iter()
            .map(|pr| visit_pr!($self, pr, $ok_visitor))
            .collect::<Vec<_>>()
            .join($sep)
    };
}

impl<'a> Visitor<String> for AstLikePP<'a> {
    fn visit_ast(&mut self, ast: &AST) -> String {
        visit_pr_vec!(self, ast.stmts(), visit_stmt, "\n")
    }

    fn visit_expr(&mut self, expr: &Expr) -> String {
        match expr.node() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Ident(ident) => self.visit_ident_expr(ident),
            expr @ ExprKind::Infix(_, _, _) => self.visit_infix_expr(expr),
            expr @ ExprKind::Prefix(_, _) => self.visit_prefix_expr(expr),
            expr @ ExprKind::App(_, _) => self.visit_app_expr(expr),
            expr @ ExprKind::Block(_) => self.visit_block_expr(expr),
            expr @ ExprKind::Let(_, _, _) => self.visit_let_expr(expr),
            expr @ ExprKind::Abs(_, _) => self.visit_abs_expr(expr),
        }
    }

    fn visit_lit_expr(&mut self, lit: &Lit) -> String {
        format!("{}", lit.ppfmt(self.sess))
    }

    fn visit_ident_expr(&mut self, ident: &Ident) -> String {
        format!("{}", ident.ppfmt(self.sess))
    }

    fn visit_infix_expr(&mut self, expr: &ExprKind) -> String {
        match_kind!(expr, ExprKind::Infix(lhs, op, rhs), {
            format!(
                "{} {} {}",
                visit_pr!(self, lhs, visit_expr),
                op.ppfmt(self.sess),
                visit_pr!(self, rhs, visit_expr)
            )
        })
    }

    fn visit_prefix_expr(&mut self, expr: &ExprKind) -> String {
        match_kind!(expr, ExprKind::Prefix(op, rhs), {
            format!(
                "{}{}",
                op.ppfmt(self.sess),
                visit_pr!(self, rhs, visit_expr)
            )
        })
    }

    fn visit_abs_expr(&mut self, expr: &ExprKind) -> String {
        match_kind!(expr, ExprKind::Abs(param, body), {
            format!(
                "\\{} -> {}",
                visit_pr!(self, param, visit_ident),
                visit_pr!(self, body, visit_expr)
            )
        })
    }

    fn visit_app_expr(&mut self, expr: &ExprKind) -> String {
        match_kind!(expr, ExprKind::App(lhs, args), {
            format!(
                "{} {}",
                visit_pr!(self, lhs, visit_expr),
                visit_pr_vec!(self, args, visit_expr, " ")
            )
        })
    }

    fn visit_block_expr(&mut self, expr: &ExprKind) -> String {
        match_kind!(expr, ExprKind::Block(exprs), {
            self.indent();
            let output = visit_pr_vec!(self, exprs, visit_stmt, "\n");
            self.dedent();
            output
        })
    }

    fn visit_let_expr(&mut self, expr: &ExprKind) -> String {
        match_kind!(expr, ExprKind::Let(name, value, body), {
            format!(
                "let {} = {} in {}",
                visit_pr!(self, name, visit_ident),
                visit_pr!(self, value, visit_expr),
                visit_pr!(self, body, visit_expr)
            )
        })
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> String {
        format!(
            "{}{}",
            self.cur_indent(),
            match stmt.node() {
                StmtKind::Expr(expr) => visit_pr!(self, expr, visit_expr),
            }
        )
    }

    fn visit_ident(&mut self, ident: &Ident) -> String {
        ident.ppfmt(self.sess)
    }
}
