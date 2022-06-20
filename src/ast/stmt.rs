use std::fmt::Display;

use crate::span::span::Spanned;

use super::{expr::Expr, N, PR};

pub type Stmt = Spanned<StmtKind>;

pub enum StmtKind {
    Expr(PR<N<Expr>>),
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                StmtKind::Expr(_) => todo!(),
            }
        )
    }
}
