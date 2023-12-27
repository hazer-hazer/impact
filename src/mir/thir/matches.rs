use super::{build::ThirBuilder, DecisionTree, Projection};
use crate::hir::{self, Map};

impl<'ctx> ThirBuilder<'ctx> {
    // pub fn match_expr(&mut self, subject: hir::Expr, arms: &[hir::expr::Arm]) ->
    // DecisionTree {}

    // pub fn arm(&mut self, subject: hir::Expr, arm: hir::expr::Arm) ->
    // DecisionTree {

    // }

    // pub fn match_expr_pat(&mut self, subject: hir::Expr, pat)

    // // pub fn project_pat_to_expr(&mut self, subject: hir::Expr, pat: hir::Pat)
    // -> Projection { //     assert_eq!(self.sess.tyctx.tyof(subject),
    // self.sess.tyctx.tyof(pat)); //     match self.hir.pat(pat).kind() {
    // //         hir::pat::PatKind::Unit => Projection::None,
    // //         hir::pat::PatKind::Ident(..) => Projection::None,
    // //         hir::pat::PatKind::Struct(..) => todo!(),
    // //         hir::pat::PatKind::Or(..) => todo!(),
    // //         hir::pat::PatKind::Tuple(_) => todo!(),
    // //     }
    // // }
}
