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
}

// impl<'a> PP<'a> for StmtKind {
//     fn ppfmt(&self, sess: &'a crate::session::Session) -> String {
//         match self {
//             StmtKind::Expr(expr) => expr.ppfmt(sess),
//             StmtKind::Let(def) => def.ppfmt(sess),
//         }
//     }
// }

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
