use crate::{
    span::span::{Spanned},
};

use super::{expr::Expr, N, PR};

pub type Stmt = Spanned<StmtKind>;

pub enum StmtKind {
    Expr(PR<N<Expr>>),
}
