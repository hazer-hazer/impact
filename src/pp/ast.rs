use crate::{
    parser::ast::{
        expr::{Expr, ExprKind, Lit},
        stmt::{LetStmt, Stmt, StmtKind},
        visitor::Visitor,
        ErrorNode, AST,
    },
    pp::PP,
    session::Session,
    span::span::Ident,
};

pub struct AstPP<'a> {
    indent_level: u32,
    sess: &'a Session,
}

impl<'a> AstPP<'a> {
    pub fn new(sess: &'a Session) -> Self {
        Self {
            indent_level: 0,
            sess,
        }
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        assert_ne!(self.indent_level, 0);
        self.indent_level -= 1;
    }

    fn print_indent(&self) {
        print!("{}", "    ".repeat(self.indent_level as usize));
    }

    fn visit_err(&self, err: &ErrorNode) {
        print!("[ERROR]");
    }
}

macro_rules! visit_pr {
    ($self: ident, $pr: expr, $ok_visitor: ident) => {
        match $pr {
            Ok(ok) => $self.$ok_visitor(ok),
            Err(err) => $self.visit_err(err),
        };
    };
}

macro_rules! match_kind {
    ($kind: expr, $should_be: pat, $visit: expr) => {
        match $kind {
            kind @ $should_be => $visit,
            _ => unreachable!(),
        }
    };
}

impl<'a> Visitor<()> for AstPP<'a> {
    fn visit_ast(&mut self, ast: &AST) {
        for stmt in ast.stmts() {
            visit_pr!(self, stmt, visit_stmt);
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr.node() {
            ExprKind::Lit(lit) => self.visit_lit_expr(lit),
            ExprKind::Ident(ident) => self.visit_ident_expr(ident),
            expr @ ExprKind::Infix(_, _, _) => self.visit_infix_expr(expr),
            expr @ ExprKind::Prefix(_, _) => self.visit_prefix_expr(expr),
            expr @ ExprKind::App(_, _) => self.visit_app_expr(expr),
        }
    }

    fn visit_lit_expr(&mut self, lit: &Lit) {
        print!("{}", lit.ppfmt(self.sess));
    }

    fn visit_ident_expr(&mut self, ident: &Ident) {
        print!("{}", ident.ppfmt(self.sess));
    }

    fn visit_infix_expr(&mut self, expr: &ExprKind) {
        match_kind!(expr, ExprKind::Infix(lhs, op, rhs), {
            visit_pr!(self, lhs, visit_expr);
            print!(" {} ", op.ppfmt(self.sess));
            visit_pr!(self, rhs, visit_expr);
        })
    }

    fn visit_prefix_expr(&mut self, expr: &ExprKind) {
        match_kind!(expr, ExprKind::Prefix(op, rhs), {
            print!("{}", op.ppfmt(self.sess));
            visit_pr!(self, rhs, visit_expr);
        })
    }

    fn visit_app_expr(&mut self, expr: &ExprKind) {
        match_kind!(expr, ExprKind::App(lhs, args), {
            visit_pr!(self, lhs, visit_expr);

            for arg in args {
                print!(" ");
                visit_pr!(self, arg, visit_expr);
            }
        })
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        self.print_indent();
        match stmt.node() {
            StmtKind::Expr(expr) => visit_pr!(self, expr, visit_expr),
            StmtKind::Let(stmt) => self.visit_let_stmt(stmt),
        }
        print!("\n");
    }

    fn visit_let_stmt(&mut self, stmt: &LetStmt) {
        print!("let ");
        visit_pr!(self, stmt.name(), visit_ident);

        print!(
            "{}",
            stmt.params()
                .iter()
                .map(|param| param.ppfmt(self.sess))
                .collect::<Vec<_>>()
                .join(" ")
        );

        print!(" = ");

        visit_pr!(self, stmt.value(), visit_expr);
    }

    fn visit_ident(&mut self, ident: &Ident) {
        print!("{}", ident.ppfmt(self.sess));
    }
}
