use std::collections::HashMap;

use crate::{
    dt::idx::{Idx, IndexVec},
    hir::{BodyId, BodyOwnerKind, HirId, OwnerId, WithHirId, HIR},
    resolve::def::DefId,
    thir::{
        build::ThirBuilder, BlockId, ExprCategory, ExprId, ExprKind, Lit, LocalVar, ParamId,
        PatKind, THIR,
    },
    typeck::tyctx::TyCtx,
};

use super::{
    scalar::Scalar, BBWith, Body, BodyBuilder, Const, LValue, Local, LocalInfo, Operand, RValue,
    Stmt, StmtKind, BB, START_BB,
};

macro_rules! unpack {
    ($bb_name: ident = $bb_with: expr) => {{
        let BBWith(bb, with) = $bb_with;
        $bb_name = bb;
        with
    }};

    ($bb_with: expr) => {{
        let BBWith(bb, ()) = $bb_with;
        bb
    }};
}

pub struct MirBuilder<'ctx> {
    thir_entry_expr: ExprId,
    thir: THIR,
    builder: BodyBuilder,
    locals: HashMap<LocalVar, Local>,

    hir: &'ctx HIR,
    tyctx: &'ctx TyCtx,
}

impl<'ctx> MirBuilder<'ctx> {
    pub fn build(body_owner: OwnerId, hir: &'ctx HIR, tyctx: &'ctx TyCtx) -> Body {
        let (thir, thir_entry_expr) = ThirBuilder::new(hir, tyctx, body_owner).build_body_thir();

        let this = Self {
            thir_entry_expr,
            thir,
            builder: BodyBuilder::default(),
            locals: Default::default(),
            hir,
            tyctx,
        };

        match this.hir.body_owner_kind(this.thir.body_owner()) {
            BodyOwnerKind::Func | BodyOwnerKind::Lambda => this.func(),
        }
    }

    fn add_local(&mut self, var: LocalVar, local: Local) {
        assert!(self.locals.insert(var, local).is_none());
    }

    fn resolve_local(&mut self, var: LocalVar) -> Local {
        self.locals.get(&var).copied().unwrap()
    }

    pub fn func(mut self) -> Body {
        self.push_return_lvalue();

        // Note: Only one param for now
        [ParamId::new(0)].into_iter().for_each(|param| {
            self.push_param(param);
        });

        self.store_expr(START_BB, LValue::return_lvalue(), self.thir_entry_expr);

        self.builder.emit()
    }

    fn block(&mut self, bb: BB, block_id: BlockId) -> BBWith<()> {
        
    }

    fn push_return_lvalue(&mut self) {
        assert_eq!(self.builder.local(LocalInfo {}), Local::return_local());
    }

    fn push_param(&mut self, param_id: ParamId) {
        let param_local = self.builder.local(LocalInfo {});
        assert_eq!(param_local.inner(), param_id.inner());

        match self.thir.param(param_id).pat.kind {
            PatKind::Unit => todo!(),
            PatKind::Ident(_, var) => {
                self.add_local(var.into(), param_local);
            },
        }
    }

    fn as_operand(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<Operand> {
        let expr = self.thir.expr(expr_id);
        match expr.categorize() {
            ExprCategory::Const => bb.with(Operand::Const(self.as_const(bb, expr_id))),
            ExprCategory::LValue | ExprCategory::RValue => {
                let operand = unpack!(bb = self.as_temp(bb, expr_id));
                bb.with(Operand::LValue(LValue::new(operand)))
            },
        }
    }

    fn as_const(&mut self, _bb: BB, expr: ExprId) -> Const {
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

    fn as_lvalue(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<LValue> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            &ExprKind::LocalRef(var) => bb.with(LValue::new(self.resolve_local(var))),
            ExprKind::Lit(_)
            | ExprKind::Block(_)
            | ExprKind::Call { .. }
            | ExprKind::Lambda { .. }
            | ExprKind::Ty(_, _)
            | ExprKind::Builtin(_) => {
                let temp = unpack!(bb = self.as_temp(bb, expr_id));
                bb.with(LValue::new(temp))
            },
        }
    }

    fn as_rvalue(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<RValue> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            ExprKind::Lit(_) => {
                bb.with(RValue::Operand(Operand::Const(self.as_const(bb, expr_id))))
            },
            ExprKind::Ty(_, _) | ExprKind::Block(_) | ExprKind::LocalRef(_) => {
                assert!(!matches!(
                    expr.categorize(),
                    ExprCategory::RValue | ExprCategory::Const
                ));

                let BBWith(bb, operand) = self.as_operand(bb, expr_id);
                bb.with(RValue::Operand(operand))
            },
            &ExprKind::Call {
                func_ty: _,
                lhs,
                arg,
            } => {
                let lhs = unpack!(bb = self.as_operand(bb, lhs));
                let arg = unpack!(bb = self.as_operand(bb, arg));

                let dest = self.temp();

                let target = self.builder.begin_bb();

                // FIXME: Calls should be terminators?
                self.push_assign(bb, dest, RValue::Call { lhs, arg, target });

                bb.with(RValue::Operand(Operand::LValue(dest)))
            },
            ExprKind::Lambda { body_id: _ } => todo!(),
            ExprKind::Builtin(_) => todo!(),
        }
    }

    fn as_temp(&mut self, bb: BB, expr: ExprId) -> BBWith<Local> {
        let local_info = LocalInfo {};
        let local = self.builder.local(local_info);

        let lvalue = LValue::new(local);

        let BBWith(bb, ()) = self.store_expr(bb, lvalue, expr);

        bb.with(local)
    }

    /// Assigns expression to lvalue
    fn store_expr(&mut self, mut bb: BB, lvalue: LValue, expr_id: ExprId) -> BBWith<()> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            ExprKind::Lit(_) => todo!(),
            ExprKind::LocalRef(_) => {
                assert_eq!(expr.categorize(), ExprCategory::LValue);

                let lvalue = unpack!(bb = self.as_lvalue(bb, expr_id));
                let rvalue = RValue::Operand(Operand::LValue(lvalue));

                self.push_assign(bb, lvalue, rvalue);
                bb.unit()
            },
            &ExprKind::Block(block_id) => self.block(bb, block_id),
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

    fn push_assign(&mut self, bb: BB, lvalue: LValue, rvalue: RValue) {
        self.builder
            .push_stmt(bb, Stmt::new(StmtKind::Assign(lvalue, rvalue)));
    }

    fn temp(&mut self) -> LValue {
        LValue::new(self.builder.local(LocalInfo {}))
    }
}
