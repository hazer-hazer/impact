use crate::span::span::{Span, Spanned};

use super::expr::Expr;

pub type Stmt<'a> = Spanned<StmtKind<'a>>;

pub enum StmtKind<'a> {
    Expr(Expr<'a>),
}
