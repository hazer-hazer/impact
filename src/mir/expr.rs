use crate::mir::thir::Expr;

use super::{
    build::{unpack, MirBuilder},
    scalar::Scalar,
    thir::{ExprCategory, ExprId, ExprKind, Lit, Pat, PatKind},
    BBWith, Const, LValue, Local, LocalInfo, Operand, RValue, BB,
};

impl<'ctx> MirBuilder<'ctx> {
    pub(super) fn as_operand(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<Operand> {
        let expr = self.thir.expr(expr_id);
        match expr.categorize() {
            ExprCategory::Const => bb.with(self.as_const(bb, expr_id).operand()),
            ExprCategory::LValue | ExprCategory::RValue => {
                let operand = unpack!(bb = self.as_temp(bb, expr_id));
                bb.with(operand.lvalue().operand())
            },
        }
    }

    pub(super) fn as_const(&mut self, _bb: BB, expr: ExprId) -> Const {
        match &self.thir.expr(expr).kind {
            ExprKind::Lit(lit) => match lit {
                &Lit::Bool(val) => Const::Scalar(Scalar::from(val)),
                &Lit::Int(val, kind) => Const::Scalar(Scalar::new(val, kind.bytes())),
                Lit::Float(val, kind) => Const::Scalar(Scalar::new(val.to_bits(), kind.bytes())),
                Lit::String(_) => todo!(),
            },
            _ => panic!(),
        }
    }

    pub(super) fn as_lvalue(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<LValue> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            &ExprKind::LocalRef(var) => bb.with(self.resolve_local_var(var).lvalue()),
            ExprKind::Lit(_)
            | ExprKind::Block(_)
            | ExprKind::Call { .. }
            | ExprKind::Lambda { .. }
            | ExprKind::Ty(_, _)
            | ExprKind::Builtin(_) => {
                let temp = unpack!(bb = self.as_temp(bb, expr_id));
                bb.with(temp.lvalue())
            },
        }
    }

    pub(super) fn as_rvalue(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<RValue> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            ExprKind::Lit(_) => bb.with(self.as_const(bb, expr_id).operand().rvalue()),
            ExprKind::Ty(_, _) | ExprKind::Block(_) | ExprKind::LocalRef(_) => {
                assert!(!matches!(
                    expr.categorize(),
                    ExprCategory::RValue | ExprCategory::Const
                ));

                let operand = unpack!(bb = self.as_operand(bb, expr_id));

                bb.with(operand.rvalue())
            },
            &ExprKind::Call { func_ty, lhs, arg } => {
                let lhs = unpack!(bb = self.as_operand(bb, lhs));
                let arg = unpack!(bb = self.as_operand(bb, arg));

                let dest = self.temp_lvalue(func_ty.return_ty(), expr.span);

                let target = self.builder.begin_bb();

                // FIXME: Calls should be terminators?
                self.push_assign(bb, dest, RValue::Call { lhs, arg, target });

                bb.with(dest.operand().rvalue())
            },
            ExprKind::Lambda { body_id: _ } => todo!(),
            ExprKind::Builtin(_) => todo!(),
        }
    }

    pub(super) fn as_temp(&mut self, bb: BB, expr_id: ExprId) -> BBWith<Local> {
        let &Expr { ty, span, .. } = self.thir.expr(expr_id);
        let temp = self.temp_lvalue(ty, span);

        unpack!(self.store_expr(bb, temp, expr_id));

        bb.with(temp.local)
    }

    /// Assigns expression to lvalue
    pub(super) fn store_expr(&mut self, mut bb: BB, lvalue: LValue, expr_id: ExprId) -> BBWith<()> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            ExprKind::Lit(_) => todo!(),
            ExprKind::LocalRef(_) => {
                assert_eq!(expr.categorize(), ExprCategory::LValue);

                let lvalue = unpack!(bb = self.as_lvalue(bb, expr_id));
                let rvalue = lvalue.operand().rvalue();

                self.push_assign(bb, lvalue, rvalue);
                bb.unit()
            },
            &ExprKind::Block(block_id) => self.block(bb, block_id, lvalue),
            &ExprKind::Call {
                func_ty: _,
                lhs,
                arg,
            } => {
                let lhs = unpack!(bb = self.as_operand(bb, lhs));
                let arg = unpack!(bb = self.as_operand(bb, arg));

                let target = self.builder.begin_bb();

                // FIXME: Calls should be terminators?
                self.push_assign(bb, lvalue, RValue::Call { lhs, arg, target });

                target.unit()
            },
            ExprKind::Lambda { .. } | ExprKind::Ty(_, _) | ExprKind::Builtin(_) => {
                assert!(!matches!(
                    expr.categorize(),
                    ExprCategory::RValue | ExprCategory::LValue
                ));
                let rvalue = unpack!(bb = self.as_rvalue(bb, expr_id));
                self.push_assign(bb, lvalue, rvalue);
                bb.unit()
            },
        }
    }

    pub(super) fn store_expr_in_pat(
        &mut self,
        mut bb: BB,
        pat: &Pat,
        expr_id: ExprId,
    ) -> BBWith<()> {
        match pat.kind {
            PatKind::Unit => todo!(),
            PatKind::Ident { name, var, ty } => todo!(),
        }
    }
}
