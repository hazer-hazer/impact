use crate::{
    ast::{
        expr::{Expr, ExprKind, Lit, PrefixOp},
        stmt::{Stmt, StmtKind},
        ty::{LitTy, Ty, TyKind},
        visitor::AstVisitor,
        AST, N, PR,
    },
    span::span::Ident,
};

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

    fn visit_stmt(&mut self, stmt: &Stmt) -> String {
        format!(
            "{}{}",
            self.cur_indent(),
            match stmt.node() {
                StmtKind::Expr(expr) => visit_pr!(self, expr, visit_expr),
            }
        )
    }

    // Expressions //
    fn visit_expr(&mut self, expr: &Expr) -> String {
        match expr.node() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Ident(ident) => self.visit_ident_expr(ident),
            ExprKind::Infix(lhs, op, rhs) => self.visit_infix_expr(lhs, op, rhs),
            ExprKind::Prefix(op, rhs) => self.visit_prefix_expr(op, rhs),
            ExprKind::App(lhs, arg) => self.visit_app_expr(lhs, arg),
            ExprKind::Block(stmts) => self.visit_block_expr(stmts),
            ExprKind::Let(name, value, body) => self.visit_let_expr(name, value, body),
            ExprKind::Abs(param, body) => self.visit_abs_expr(param, body),
            ExprKind::Ty(expr, ty) => self.visit_type_expr(expr, ty),
        }
    }

    fn visit_lit_expr(&mut self, lit: &Lit) -> String {
        format!("{}", lit)
    }

    fn visit_ident_expr(&mut self, ident: &Ident) -> String {
        format!("{}", ident)
    }

    fn visit_infix_expr(
        &mut self,
        lhs: &PR<N<Expr>>,
        op: &crate::ast::expr::InfixOp,
        rhs: &PR<N<Expr>>,
    ) -> String {
        format!(
            "{} {} {}",
            visit_pr!(self, lhs, visit_expr),
            op,
            visit_pr!(self, rhs, visit_expr)
        )
    }

    fn visit_prefix_expr(&mut self, op: &PrefixOp, rhs: &PR<N<Expr>>) -> String {
        format!(
            "{}{}",
            op,
            visit_pr!(self, rhs, visit_expr)
        )
    }

    fn visit_abs_expr(&mut self, param: &PR<Ident>, body: &PR<N<Expr>>) -> String {
        format!(
            "\\{} -> {}",
            visit_pr!(self, param, visit_ident),
            visit_pr!(self, body, visit_expr)
        )
    }

    fn visit_app_expr(&mut self, lhs: &PR<N<Expr>>, arg: &PR<N<Expr>>) -> String {
        format!(
            "{} {}",
            visit_pr!(self, lhs, visit_expr),
            visit_pr!(self, arg, visit_expr)
        )
    }

    fn visit_block_expr(&mut self, stmts: &Vec<PR<N<Stmt>>>) -> String {
        self.indent();
        let output = visit_pr_vec!(self, stmts, visit_stmt, "\n");
        self.dedent();
        output
    }

    fn visit_let_expr(
        &mut self,
        name: &PR<Ident>,
        value: &PR<N<Expr>>,
        body: &PR<N<Expr>>,
    ) -> String {
        format!(
            "let {} = {} in {}",
            visit_pr!(self, name, visit_ident),
            visit_pr!(self, value, visit_expr),
            visit_pr!(self, body, visit_expr)
        )
    }

    fn visit_type_expr(&mut self, expr: &PR<N<Expr>>, ty: &PR<N<Ty>>) -> String {
        format!(
            "{}: {}",
            visit_pr!(self, expr, visit_expr),
            visit_pr!(self, ty, visit_ty)
        )
    }

    // Types //
    fn visit_ty(&mut self, ty: &Ty) -> String {
        match ty.node() {
            TyKind::Lit(lit_ty) => self.visit_lit_ty(lit_ty),
            TyKind::Unit => self.visit_unit_ty(),
            TyKind::Var(ident) => self.visit_var_ty(ident),
            TyKind::Func(param_ty, return_ty) => self.visit_func_ty(param_ty, return_ty),
            TyKind::Paren(inner) => self.visit_paren_ty(inner),
        }
    }

    fn visit_unit_ty(&mut self) -> String {
        "()".to_string()
    }

    fn visit_lit_ty(&mut self, lit_ty: &LitTy) -> String {
        format!("{}", lit_ty)
    }

    fn visit_var_ty(&mut self, ident: &PR<Ident>) -> String {
        visit_pr!(self, ident, visit_ident)
    }

    fn visit_func_ty(&mut self, param_ty: &PR<N<Ty>>, return_ty: &PR<N<Ty>>) -> String {
        format!(
            "{} -> {}",
            visit_pr!(self, param_ty, visit_ty),
            visit_pr!(self, return_ty, visit_ty)
        )
    }

    fn visit_paren_ty(&mut self, inner: &PR<N<Ty>>) -> String {
        format!("({})", visit_pr!(self, inner, visit_ty))
    }

    // Fragments //
    fn visit_ident(&mut self, ident: &Ident) -> String {
        ident.to_string()
    }
}
