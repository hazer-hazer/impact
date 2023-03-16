use crate::{
    mir::thir::Expr,
    typeck::ty::{FloatKind, IntKind},
};

use super::{
    build::{unpack, MirBuilder},
    scalar::Scalar,
    thir::{ExprCategory, ExprId, ExprKind, Lit, Pat, PatKind},
    BBWith, Const, LValue, Local, Operand, RValue, Ty, BB,
};

impl<'ctx> MirBuilder<'ctx> {
    // Constants (literals) //
    pub(super) fn bool_const(&self, val: bool) -> Const {
        Const::scalar(Ty::bool(), Scalar::from(val))
    }

    pub(super) fn unit_const(&self) -> Const {
        Const::zero_sized(Ty::unit())
    }

    pub(super) fn int_const(&self, val: u64, kind: IntKind) -> Const {
        Const::scalar(Ty::int(kind), Scalar::new(val, kind.bytes()))
    }

    pub(super) fn float_const(&self, val: f64, kind: FloatKind) -> Const {
        Const::scalar(Ty::float(kind), Scalar::new(val.to_bits(), kind.bytes()))
    }

    // Expressions as categories //
    pub(super) fn as_operand(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<Operand> {
        let expr = self.thir.expr(expr_id);
        match expr.categorize() {
            ExprCategory::Const => bb.with(self.as_const(bb, expr_id).operand()),
            ExprCategory::LValue | ExprCategory::AsRValue | ExprCategory::StoreRValue => {
                let operand = unpack!(bb = self.as_temp(bb, expr_id));
                bb.with(operand.lvalue().operand())
            },
        }
    }

    pub(super) fn as_const(&mut self, _bb: BB, expr: ExprId) -> Const {
        match &self.thir.expr(expr).kind {
            ExprKind::Lit(lit) => match lit {
                &Lit::Bool(val) => self.bool_const(val),
                &Lit::Int(val, kind) => self.int_const(val, kind),
                &Lit::Float(val, kind) => self.float_const(val, kind),
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
            | ExprKind::Def(_, _)
            | ExprKind::Builtin(_) => {
                let temp = unpack!(bb = self.as_temp(bb, expr_id));
                bb.with(temp.lvalue())
            },
        }
    }

    pub(super) fn as_rvalue(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<RValue> {
        let expr = self.thir.expr(expr_id);
        let expr_span = expr.span;
        match &expr.kind {
            ExprKind::Lit(_) => bb.with(self.as_const(bb, expr_id).operand().rvalue()),

            // FIXME: We need to gen cast from ascription?
            &ExprKind::Ty(expr_id, _) => self.as_rvalue(bb, expr_id),

            ExprKind::Block(_) | ExprKind::LocalRef(_) => {
                assert!(!matches!(
                    expr.categorize(),
                    ExprCategory::AsRValue | ExprCategory::Const
                ));

                let operand = unpack!(bb = self.as_operand(bb, expr_id));

                bb.with(operand.rvalue())
            },
            &ExprKind::Call { func_ty, lhs, arg } => {
                let lhs = unpack!(bb = self.as_operand(bb, lhs));
                let arg = unpack!(bb = self.as_operand(bb, arg));

                let dest = self.temp_lvalue(func_ty.return_ty(), expr_span);

                // let target = self.builder.begin_bb();

                // FIXME: Calls should be terminators?
                self.push_assign(bb, dest, RValue::Call { lhs, arg });

                // self.builder.goto(bb, target);

                bb.with(dest.operand().rvalue())
            },
            &ExprKind::Lambda { body_id, def_id } => {
                self.builder.references_body(body_id);
                bb.with(RValue::Closure(def_id))
            },
            &ExprKind::Def(def_id, ty) => bb.with(RValue::Def(def_id, ty)),

            ExprKind::Builtin(_) => todo!(),
        }
    }

    pub(super) fn as_temp(&mut self, bb: BB, expr_id: ExprId) -> BBWith<Local> {
        let &Expr { ty, span, .. } = self.thir.expr(expr_id);
        let temp = self.temp_lvalue(ty, span);

        unpack!(self.store_expr(bb, temp, expr_id));

        bb.with(temp.local)
    }

    // Assigning expressions to ... //

    /// Assigns expression to lvalue
    pub(super) fn store_expr(&mut self, mut bb: BB, lvalue: LValue, expr_id: ExprId) -> BBWith<()> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            // FIXME
            &ExprKind::Ty(expr_id, _) => self.store_expr(bb, lvalue, expr_id),

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

                // let target = self.builder.begin_bb();

                // FIXME: Calls should be terminators?
                self.push_assign(bb, lvalue, RValue::Call { lhs, arg });

                // self.builder.goto(bb, target);
                // target.unit()

                bb.unit()
            },

            ExprKind::Lit(_)
            | ExprKind::Def(_, _)
            | ExprKind::Lambda { .. }
            | ExprKind::Builtin(_) => {
                assert!(!matches!(
                    expr.categorize(),
                    ExprCategory::StoreRValue | ExprCategory::LValue
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
            PatKind::Ident { var, .. } => {
                let local = self.resolve_local_var(var);
                let rvalue = unpack!(bb = self.as_rvalue(bb, expr_id));
                self.push_assign(bb, local.lvalue(), rvalue);
            },
        }
        bb.unit()
    }
}
