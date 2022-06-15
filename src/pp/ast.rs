use std::fmt::format;

use crate::{
    ast::{
        expr::{Expr, ExprKind, Lit},
        stmt::{Stmt, StmtKind},
        visitor::AstVisitor, AST, ty::{Type, TypeKind, LitType}, PR, N,
    },
    pp::PP,
    span::span::Ident,
};

use super::match_kind;
use super::AstLikePP;

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

impl<'a> AstVisitor<String> for AstLikePP<'a> {
    fn visit_ast(&mut self, ast: &AST) -> String {
        println!("=== AST ===");
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
            expr @ ExprKind::Type(_, _) => self.visit_type_expr(expr),
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
        match_kind!(expr, ExprKind::App(lhs, arg), {
            format!(
                "{} {}",
                visit_pr!(self, lhs, visit_expr),
                visit_pr!(self, arg, visit_expr)
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

    fn visit_type_expr(&mut self, expr: &ExprKind) -> String {
        match_kind!(expr, ExprKind::Type(expr, ty), {
            format!("{}: {}", visit_pr!(self, expr, visit_expr), visit_pr!(self, ty, visit_ty))
        })
    }

    fn visit_ty(&mut self, ty: &Type) -> String {
        match ty.node() {
            TypeKind::Lit(lit_ty) => self.visit_lit_ty(lit_ty),

            TypeKind::Unit => self.visit_unit_ty(),

            TypeKind::Var(ident) => self.visit_var_ty(ident),

            TypeKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
        }
    }

    fn visit_unit_ty(&mut self) -> String {
        "()".to_string()
    }

    fn visit_lit_ty(&mut self, lit_ty: &LitType) -> String {
        format!("{}", lit_ty)
    }

    fn visit_var_ty(&mut self, ident: &Ident) -> String {
        self.visit_ident(ident)
    }

    fn visit_func_ty(&mut self, param_ty: &PR<N<Type>>, return_ty: &PR<N<Type>>) -> String {
        format!("{} -> {}", visit_pr!(self, param_ty, visit_ty), visit_pr!(self, return_ty, visit_ty))
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
