use crate::{
    pp::PP,
    span::span::{Ident, Span},
};

use super::expr::Expr;

pub struct Stmt {
    span: Span,
    kind: StmtKind,
}

pub enum StmtKind {
    Expr(Expr),
    Let(LetStmt),
}

pub struct LetStmt {
    name: Ident,
    params: Vec<Ident>,
    value: Expr,
}

impl LetStmt {
    pub fn new(name: Ident, params: Vec<Ident>, value: Expr) -> Self {
        Self {
            name,
            params,
            value,
        }
    }

    pub fn is_var(&self) -> bool {
        self.params.is_empty()
    }
}

impl<'a> PP<'a> for Stmt {
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        self.kind.ppfmt(sess)
    }
}

impl<'a> PP<'a> for StmtKind {
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        match self {
            StmtKind::Expr(expr) => expr.ppfmt(sess),
            StmtKind::Let(def) => def.ppfmt(sess),
        }
    }
}

impl<'a> PP<'a> for LetStmt {
    fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
        format!(
            "{} {}",
            self.name.ppfmt(sess),
            self.params
                .iter()
                .map(|p| p.ppfmt(sess))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}
