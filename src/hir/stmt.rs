use crate::span::span::{Spanned};

use super::expr::Expr;

pub type Stmt = Spanned<StmtKind>;

pub enum StmtKind {
    Expr(Expr),
}
