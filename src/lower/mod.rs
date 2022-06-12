use crate::{
    ast::{
        expr::{Expr, ExprKind, Lit},
        stmt::{self, Stmt, StmtKind},
        AST,
    },
    hir::{self, HIR},
    message::message::MessageStorage,
    pp::match_kind,
    session::{OkStageResult, Session, Stage, StageResult},
    span::span::Ident,
};

macro_rules! lower_pr_boxed {
    ($self: ident, $pr: expr, $lower: ident) => {
        match $pr {
            Ok(node) => Box::new($self.$lower(node)),
            Err(err) => panic!("Error node on lower stage {}", err),
        }
    };
}

macro_rules! lower_pr {
    ($self: ident, $pr: expr, $lower: ident) => {
        match $pr {
            Ok(node) => $self.$lower(node),
            Err(err) => panic!("Error node on lower stage {}", err),
        }
    };
}

macro_rules! lower_each_pr_boxed {
    ($self: ident, $prs: expr, $lower: ident) => {
        $prs.iter()
            .map(|pr| lower_pr_boxed!($self, pr, $lower))
            .collect::<Vec<_>>()
    };
}

macro_rules! lower_each_pr {
    ($self: ident, $prs: expr, $lower: ident) => {
        $prs.iter()
            .map(|pr| lower_pr!($self, pr, $lower))
            .collect::<Vec<_>>()
    };
}

pub struct Lower<'a> {
    sess: Session,
    ast: &'a AST,
    msg: MessageStorage,
}

impl<'a> Lower<'a> {
    pub fn new(sess: Session, ast: &'a AST) -> Self {
        Self {
            sess,
            ast,
            msg: Default::default(),
        }
    }

    fn lower_ast(&mut self) -> HIR {
        HIR::new(lower_each_pr_boxed!(self, self.ast.stmts(), lower_stmt))
    }

    fn lower_expr(&mut self, expr: &Expr) -> hir::expr::Expr {
        let kind = match expr.node() {
            ExprKind::Lit(lit) => self.lower_lit_expr(lit),
            ExprKind::Ident(ident) => self.lower_ident_expr(ident),
            expr @ ExprKind::Infix(_, _, _) => self.lower_infix_expr(expr),
            expr @ ExprKind::Prefix(_, _) => self.lower_prefix_expr(expr),
            expr @ ExprKind::Abs(_, _) => self.lower_abs_expr(expr),
            expr @ ExprKind::App(_, _) => self.lower_app_expr(expr),
            expr @ ExprKind::Block(_) => self.lower_block_expr(expr),
            expr @ ExprKind::Let(_, _, _) => self.lower_let_expr(expr),
        };

        hir::expr::Expr::new(expr.span(), kind)
    }

    fn lower_lit_expr(&mut self, lit: &Lit) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Lit(*lit)
    }

    fn lower_ident_expr(&mut self, ident: &Ident) -> hir::expr::ExprKind {
        hir::expr::ExprKind::Ident(*ident)
    }

    fn lower_infix_expr(&mut self, expr: &ExprKind) -> hir::expr::ExprKind {
        match_kind!(expr, ExprKind::Infix(lhs, op, rhs), {
            hir::expr::ExprKind::Infix(
                lower_pr_boxed!(self, lhs, lower_expr),
                op.clone(),
                lower_pr_boxed!(self, rhs, lower_expr),
            )
        })
    }

    fn lower_prefix_expr(&mut self, expr: &ExprKind) -> hir::expr::ExprKind {
        match_kind!(expr, ExprKind::Prefix(op, rhs), {
            hir::expr::ExprKind::Prefix(op.clone(), lower_pr_boxed!(self, rhs, lower_expr))
        })
    }

    fn lower_abs_expr(&mut self, expr: &ExprKind) -> hir::expr::ExprKind {
        match_kind!(expr, ExprKind::Abs(param, body), {
            hir::expr::ExprKind::Abs(
                lower_pr!(self, param, lower_ident),
                lower_pr_boxed!(self, body, lower_expr),
            )
        })
    }

    fn lower_app_expr(&mut self, expr: &ExprKind) -> hir::expr::ExprKind {
        match_kind!(expr, ExprKind::App(lhs, arg), {
            hir::expr::ExprKind::App(
                lower_pr_boxed!(self, lhs, lower_expr),
                lower_pr_boxed!(self, arg, lower_expr),
            )
        })
    }

    fn lower_block_expr(&mut self, expr: &ExprKind) -> hir::expr::ExprKind {
        match_kind!(expr, ExprKind::Block(stmts), {
            hir::expr::ExprKind::Block(lower_each_pr!(self, stmts, lower_stmt))
        })
    }

    fn lower_let_expr(&mut self, expr: &ExprKind) -> hir::expr::ExprKind {
        match_kind!(expr, ExprKind::Let(name, value, body), {
            hir::expr::ExprKind::Let(
                lower_pr!(self, name, lower_ident),
                lower_pr_boxed!(self, value, lower_expr),
                lower_pr_boxed!(self, body, lower_expr),
            )
        })
    }

    fn lower_stmt(&mut self, stmt: &Stmt) -> hir::stmt::Stmt {
        match stmt.node() {
            StmtKind::Expr(expr) => hir::stmt::Stmt::new(
                stmt.span(),
                hir::stmt::StmtKind::Expr(lower_pr!(self, expr, lower_expr)),
            ),
        }
    }

    fn lower_ident(&mut self, ident: &Ident) -> Ident {
        *ident
    }
}

impl<'a> Stage<HIR> for Lower<'a> {
    fn run(mut self) -> StageResult<HIR> {
        let hir = self.lower_ast();
        StageResult::new(self.sess, hir, self.msg)
    }

    fn run_and_unwrap(self) -> OkStageResult<HIR> {
        self.run().unwrap()
    }
}
