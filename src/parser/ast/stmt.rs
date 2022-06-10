use crate::{
    pp::PP,
    span::span::{Ident, Span, Spanned},
};

use super::{expr::Expr, PR, N};

pub type Stmt = Spanned<StmtKind>;

pub enum StmtKind {
    Expr(PR<N<Expr>>),
    Let(LetStmt),
}

pub struct LetStmt {
    name: PR<Ident>,
    params: Vec<Ident>,
    value: PR<N<Expr>>,
}

impl LetStmt {
    pub fn new(name: PR<Ident>, params: Vec<Ident>, value: PR<N<Expr>>) -> Self { Self { name, params, value } }

    pub fn is_var(&self) -> bool {
        self.params.is_empty()
    }

    pub fn name(&self) -> &PR<Ident> {
        &self.name
    }

    pub fn params(&self) -> &Vec<Ident> {
        &self.params
    }

    pub fn value(&self) -> &PR<N<Expr>> {
        &self.value
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
