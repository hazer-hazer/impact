use crate::{
    pp::PP,
    span::span::{Ident, Spanned},
};

use super::{expr::Expr, PR, N};

pub type Stmt = Spanned<StmtKind>;

pub enum StmtKind {
    Expr(PR<N<Expr>>),
}
