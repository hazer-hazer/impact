use crate::{
    cli::verbose,
    mir::{thir::Expr, InfixOp},
    resolve::{builtin::Builtin, def::DefKind},
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
                &Lit::String(sym) => self.string_slice_const(sym.as_str().as_bytes()),
            },
            _ => panic!(),
        }
    }

    pub(super) fn as_lvalue(&mut self, mut bb: BB, expr_id: ExprId) -> BBWith<LValue> {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            &ExprKind::LocalRef(var) => bb.with(self.resolve_local_var(var).lvalue()),
            &ExprKind::FieldAccess(expr, field, variant_id) => {
                let lhs = unpack!(bb = self.as_lvalue(bb, expr));
                todo!()
            },
            ExprKind::Lit(_)
            | ExprKind::Block(_)
            | ExprKind::Call { .. }
            | ExprKind::Lambda { .. }
            | ExprKind::Ty(_, _)
            | ExprKind::Def(_, _)
            | ExprKind::Ref(_)
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

            // FIXME: We need to gen cast from ascription?
            &ExprKind::Ty(expr_id, _) => self.as_rvalue(bb, expr_id),

            ExprKind::FieldAccess(_, _, _) | ExprKind::Block(_) | ExprKind::LocalRef(_) => {
                assert!(!matches!(
                    expr.categorize(),
                    ExprCategory::AsRValue | ExprCategory::Const
                ));

                let operand = unpack!(bb = self.as_operand(bb, expr_id));

                bb.with(operand.rvalue())
            },
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
            &ExprKind::Def(def_id, ty) => {
                let rvalue = match self.sess.def_table.get_def(def_id).kind() {
                    DefKind::External => {
                        if ty.is_func_like() {
                            RValue::FuncRef(def_id, ty)
                        } else {
                            RValue::ValueRef(def_id)
                        }
                    },
                    DefKind::Func => RValue::FuncRef(def_id, ty),
                    DefKind::Value => RValue::ValueRef(def_id),
                    DefKind::Lambda => {
                        assert!(ty.is_instantiated());
                        RValue::ClosureRef(def_id)
                    },
                    DefKind::Ctor => todo!(),
                    DefKind::Local
                    | DefKind::Root
                    | DefKind::TyAlias
                    | DefKind::Data
                    | DefKind::Variant
                    | DefKind::Mod
                    | DefKind::DeclareBuiltin => {
                        unreachable!()
                    },
                    DefKind::Data => todo!(),
                    DefKind::Variant => todo!(),
                    DefKind::Ctor => todo!(),
                    DefKind::FieldAccessor => todo!(),
                };
                bb.with(rvalue)
            },

            &ExprKind::Builtin(bt) => match bt {
                Builtin::UnitValue => bb.with(self.unit_const().operand().rvalue()),
                Builtin::AddInt | Builtin::SubInt => {
                    bb.with(RValue::Infix(InfixOp::try_from(bt).unwrap()))
                    // unreachable!("builtin {} must not appear as a standalone expression", bt)
                },
                Builtin::RefTy | Builtin::Str | Builtin::UnitTy | Builtin::I32 => {
                    unreachable!("builtin {} is not an expression", bt)
                },
                Builtin::RefCons => {
                    panic!("Ref constructor used as a standalone expression and must not appear on MIR building stage as Builtin")
                },
            },
            ExprKind::Call { .. } | ExprKind::Ref(_) => {
                assert!(!matches!(
                    expr.categorize(),
                    ExprCategory::AsRValue | ExprCategory::Const
                ));
                let operand = unpack!(bb = self.as_operand(bb, expr_id));
                bb.with(operand.rvalue())
            },
        }
    }

    // fn call_as_rvalue(&mut self, bb: BB, func_ty: &Ty, lhs: &ExprId, args: &[ExprId]) -> BBWith<RValue> {}

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
            // FIXME
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

            ExprKind::Lit(_)
            | ExprKind::Def(_, _)
            | ExprKind::Lambda { .. }
            | ExprKind::FieldAccess(_, _, _)
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
                self.push_assign(bb, local.lvalue(), rvalue);
            },
        }
        bb.unit()
    }
}
