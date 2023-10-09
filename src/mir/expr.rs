use super::{
    build::{unpack, MirBuilder},
    scalar::Scalar,
    thir::{Arm, ExprCategory, ExprId, ExprKind, Lit, Pat, PatKind},
    BBWith, Const, LValue, Local, Operand, RValue, Ty, BB,
};
use crate::{
    cli::verbose,
    hir::ValueDefKind,
    mir::{thir::Expr, InfixOp},
    resolve::builtin::ValueBuiltin,
    typeck::ty::{FloatKind, IntKind},
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

    pub(super) fn string_slice_const(&self, val: &[u8]) -> Const {
        // FIXME: `to_owner` ok? Maybe we can leak as all strings are interned globally?
        Const::slice(Ty::str(), val.to_owned().into_boxed_slice())
    }

    // Expressions as categories //
    pub(super) fn as_operand(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<Operand> {
        let expr = self.thir.expr(expr_id);
        match expr.categorize() {
            ExprCategory::Const => bb.with(self.as_const(bb, expr_id).operand()),
            ExprCategory::LValue | ExprCategory::AsRValue | ExprCategory::StoreRValue => {
                let operand = unpack!(bb = self.as_temp(bb, expr_id));
                bb.with(operand.lvalue(None).operand())
            },
        }
    }

    pub(super) fn as_const(&mut self, _bb: BB, expr: ExprId) -> Const {
        match &self.thir.expr(expr).kind {
            ExprKind::Lit(lit) => match lit {
                &Lit::Bool(val) => self.bool_const(val),
                &Lit::Int(val, kind) => self.int_const(val, kind),
                &Lit::Float(val, kind) => self.float_const(val, kind),
                &Lit::String(sym) => self.string_slice_const(sym.as_str().as_bytes()),
            },
            _ => panic!(),
        }
    }

    pub(super) fn as_lvalue(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<LValue> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            &ExprKind::LocalRef(var) => bb.with(self.resolve_local_var(var).lvalue(None)),
            // &ExprKind::FieldAccess(lhs, vid, fid) => {
            //     let field_ty = self.thir.expr(lhs).ty.as_adt().unwrap().field_ty(vid, fid);
            //     let lhs = unpack!(bb = self.as_lvalue(bb, lhs));
            //     bb.with(lhs.project(Projection::field(field_ty, vid, fid)))
            // },
            ExprKind::Lit(_)
            | ExprKind::Block(_)
            | ExprKind::Call { .. }
            | ExprKind::Lambda { .. }
            | ExprKind::Ty(..)
            | ExprKind::Def(..)
            | ExprKind::Ref(_)
            | ExprKind::Match(..)
            | ExprKind::Builtin(_) => {
                let temp = unpack!(bb = self.as_temp(bb, expr_id));
                bb.with(temp.lvalue(None))
            },
        }
    }

    pub(super) fn as_rvalue(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<RValue> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            ExprKind::Lit(_) => bb.with(self.as_const(bb, expr_id).operand().rvalue()),

            // FIXME: We need to gen cast from ascription?
            &ExprKind::Ty(expr_id, _) => self.as_rvalue(bb, expr_id),

            // ExprKind::FieldAccess(..)  => {},
            // ExprKind::Call { func_ty, lhs, args } => {
            //     // FIXME: Clone
            //     let func_ty = *func_ty;
            //     let args = args.clone();
            //     let lhs = unpack!(bb = self.as_operand(bb, *lhs));
            //     let args = args
            //         .iter()
            //         .map(|arg| unpack!(bb = self.as_operand(bb, *arg)))
            //         .collect();

            //     let dest = self.temp_lvalue(func_ty.return_ty(), expr_span);

            //     // let target = self.builder.begin_bb();

            //     // FIXME: Calls should be terminators?
            //     self.push_assign(bb, dest, RValue::Call { lhs, args });

            //     // self.builder.goto(bb, target);

            //     bb.with(dest.operand().rvalue())
            // },
            &ExprKind::Lambda { body_id, def_id } => {
                self.builder.references_body(body_id);
                bb.with(RValue::Closure(def_id))
            },
            &ExprKind::Def(def_id, kind, ty) => {
                let rvalue = match kind {
                    ValueDefKind::External => {
                        if ty.is_func_like() {
                            RValue::FuncRef(def_id, ty)
                        } else {
                            RValue::ValueRef(def_id)
                        }
                    },
                    ValueDefKind::Func => RValue::FuncRef(def_id, ty),
                    ValueDefKind::Value => RValue::ValueRef(def_id),
                    // ExprDefKind::Lambda => {
                    //     assert!(ty.is_instantiated());
                    //     RValue::ClosureRef(def_id)
                    // },
                    ValueDefKind::Ctor => RValue::Ctor(def_id, ty),
                    ValueDefKind::FieldAccessor => RValue::FieldAccessor(def_id, ty),
                };
                bb.with(rvalue)
            },

            &ExprKind::Builtin(bt) => match bt {
                ValueBuiltin::UnitValue => bb.with(self.unit_const().operand().rvalue()),
                ValueBuiltin::AddInt | ValueBuiltin::SubInt => {
                    bb.with(RValue::Infix(InfixOp::try_from(bt).unwrap()))
                    // unreachable!("ValueBuiltin {} must not appear as a
                    // standalone expression", bt)
                },
                ValueBuiltin::RefCons => {
                    panic!("Ref constructor used as a standalone expression and must not appear on MIR building stage as ValueBuiltin")
                },
            },
            ExprKind::Block(_)
            | ExprKind::LocalRef(_)
            | ExprKind::Call { .. }
            | ExprKind::Match(..)
            | ExprKind::Ref(_) => {
                assert!(!matches!(
                    expr.categorize(),
                    ExprCategory::AsRValue | ExprCategory::Const
                ));
                let operand = unpack!(bb = self.as_operand(bb, expr_id));
                bb.with(operand.rvalue())
            },
        }
    }

    pub(super) fn as_temp(&mut self, bb: BB, expr_id: ExprId) -> BBWith<Local> {
        let &Expr { ty, span, .. } = self.thir.expr(expr_id);
        let temp = self.temp_lvalue(ty, span);

        unpack!(self.store_expr(bb, temp, expr_id)).with(temp.local)
    }

    // Assigning expressions to ... //

    /// Assigns expression to lvalue
    pub(super) fn store_expr(&mut self, mut bb: BB, lvalue: LValue, expr_id: ExprId) -> BBWith<()> {
        let expr = self.thir.expr(expr_id);
        verbose!("Store {lvalue} = {expr}");
        match &expr.kind {
            // FIXME: Cast logic?
            &ExprKind::Ty(expr_id, _) => self.store_expr(bb, lvalue, expr_id),

            ExprKind::LocalRef(_) => {
                assert_eq!(expr.categorize(), ExprCategory::LValue);

                let rvalue = unpack!(bb = self.as_lvalue(bb, expr_id)).operand().rvalue();

                self.push_assign(bb, lvalue, rvalue);
                bb.unit()
            },
            &ExprKind::Block(block_id) => self.block(bb, block_id, lvalue),
            ExprKind::Call {
                func_ty: _,
                lhs,
                args,
            } => {
                // FIXME: Can I remove cloning?
                let args = args.clone();
                let lhs = unpack!(bb = self.as_operand(bb, *lhs));
                let args = args
                    .iter()
                    .map(|&arg| unpack!(bb = self.as_operand(bb, arg)))
                    .collect();

                // let target = self.builder.begin_bb();

                // FIXME: Calls should be terminators?
                self.push_assign(bb, lvalue, RValue::Call { lhs, args });

                // self.builder.goto(bb, target);
                // target.unit()

                bb.unit()
            },

            &ExprKind::Ref(arg) => {
                let operand = unpack!(bb = self.as_operand(bb, arg));
                self.push_assign(bb, lvalue, operand.rvalue());
                bb.unit()
            },

            /*
             * match goes brrrr
             * 
             * match subject
             *   pat1 => body1
             *   pat2 => body2
             *   _ => bodyElse
             * 
             * bbAfter
             * 
             * 
             * Go backward?
             * 
             * For each pattern we do switch
             * switch(subject, [patN], [bodyN, bbN+1])
             * 
             * First, we make an subject operand.
             * let subject = eval subject; // make an operand
             * 
             * We have bbAfter, this is the BB we'll return from match generation.
             * Start with it, go backward.
             * 
             * 
             * Arm = {
             *   switch(subject, [])
             * }
             */
            ExprKind::Match(subject, arms) => {
                let subject = unpack!(bb = self.as_operand(bb, *subject));



                // self.builder.terminate_switch(bb, subject, targets);

                let else_bb = self.builder.begin_bb();

                // arms.iter().rev().fold((else_bb, bb), |(mut else_bb, body_bb), arm| {

                // });

                else_bb.unit()
            },

            ExprKind::Lit(_)
            | ExprKind::Def(..)
            | ExprKind::Lambda { .. }
            // | ExprKind::FieldAccess(..)
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
                verbose!("Store {local} = {rvalue}");
                self.push_assign(bb, local.lvalue(None), rvalue);
            },
        }
        bb.unit()
    }
}
