use crate::{
    pp::PP,
    span::span::{Ident, Span},
};

use super::expr::{Expr, self};

pub struct Stmt {
    span: Span,
    kind: StmtKind,
}

pub enum StmtKind {
    Expr(Expr),
    TermDef(TermDef),
}

pub struct TermDef {
    name: Ident,
    params: Vec<Ident>,
    value: Expr,
}

impl TermDef {
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
            StmtKind::TermDef(def) => def.ppfmt(sess),
        }
    }
}

impl<'a> PP<'a> for TermDef {
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
