use crate::cli::verbose;

use super::{
    build::{unpack, MirBuilder},
    thir::{ExprId, ExprKind},
    BBWith, LValue, RValue, StmtKind, BB,
};

impl<'ctx> MirBuilder<'ctx> {
    pub(super) fn push_assign(&mut self, bb: BB, lvalue: LValue, rvalue: RValue) {
        verbose!("Push assign {lvalue} = {rvalue}");
        self.builder
            .push_stmt(bb, super::Stmt::new(StmtKind::Assign(lvalue, rvalue)));
    }

    pub(super) fn push_assign_unit(&mut self, bb: BB, lvalue: LValue) {
        self.push_assign(bb, lvalue, self.unit_const().operand().rvalue())
    }

    pub(super) fn expr_stmt(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<()> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            ExprKind::Lit(_)
            | ExprKind::LocalRef(_)
            | ExprKind::Def(_, _)
            | ExprKind::Block(_)
            | ExprKind::Call { .. }
            | ExprKind::Lambda { .. }
            | ExprKind::Ty(_, _)
            | ExprKind::Ref(_)
            | ExprKind::Builtin(_) => {
                let _temp = unpack!(bb = self.as_temp(bb, expr_id));
            },
        }

        bb.unit()
    }
}
