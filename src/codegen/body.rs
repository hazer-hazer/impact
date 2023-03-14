use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    dt::idx::IndexVec,
    hir::BodyId,
    mir::{
        scalar::Scalar, Body, Const, ConstKind, Local, Operand, RValue, Stmt, StmtKind, Terminator,
        Ty, BB, MIR,
    },
    session::Session,
    typeck::ty::{FloatKind, PrimTy, TyKind},
};

use super::codegen::CodeGen;

pub struct BodyCodeGen<'ink, 'ctx> {
    sess: &'ctx Session,
    mir: &'ctx MIR,
    body_id: BodyId,
    body: &'ctx Body,

    // LLVM Context
    llvm_ctx: &'ink Context,
    builder: Builder<'ink>,
    func: FunctionValue<'ink>,

    //
    locals_pointers: IndexVec<Local, PointerValue<'ink>>,
}

impl<'ink, 'ctx> BodyCodeGen<'ink, 'ctx> {
    pub fn new(
        sess: &'ctx Session,
        mir: &'ctx MIR,
        body_id: BodyId,
        llvm_ctx: &'ink Context,
        func: FunctionValue<'ink>,
    ) -> Self {
        let builder = llvm_ctx.create_builder();

        let body_bb = llvm_ctx.append_basic_block(func, "body");
        builder.position_at_end(body_bb);

        Self {
            sess,
            mir,
            body_id,
            body: mir.expect(body_id),
            llvm_ctx,
            builder,
            func,
            locals_pointers: Default::default(),
        }
    }

    // Generators //
    pub fn gen_body(&mut self) {}

    fn gen_bb(&mut self, bb: BB) {
        let ll_bb = self
            .llvm_ctx
            .append_basic_block(self.func, &format!("bb{}", bb.inner()));
        let bb = self.body.bb(bb);

        self.builder.position_at_end(ll_bb);

        bb.stmts.iter().for_each(|stmt| self.gen_stmt(stmt));
        self.gen_terminator(&bb.terminator);
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Assign(lv, rv) => {
                let ptr = self.local_ptr(lv.local);
                let value = self.rvalue_to_value(rv);
                self.builder.build_store(ptr, value);
            },
        }
    }

    fn gen_terminator(&mut self, terminator: &Terminator) {}

    // To-value converters //
    fn rvalue_to_value(&mut self, rvalue: &RValue) -> BasicValueEnum<'ink> {
        match rvalue {
            RValue::Operand(_) => todo!(),
            RValue::Infix(_, _, _) => todo!(),
            RValue::Closure(_) => todo!(),
            RValue::Def(_, _) => todo!(),
            RValue::Call { lhs, arg } => todo!(),
        }
    }

    fn operand_to_value(&mut self, operand: &Operand) -> BasicValueEnum<'ink> {
        match operand {
            Operand::LValue(lv) => {
                // TODO: Useful info in name
                self.builder.build_load(self.local_ptr(lv.local), "load")
            },
            Operand::Const(const_) => self.const_to_value(const_),
        }
    }

    fn const_to_value(&mut self, const_: &Const) -> BasicValueEnum<'ink> {
        match const_.kind {
            ConstKind::Scalar(scalar) => match const_.ty.kind() {
                TyKind::Prim(prim) => match prim {
                    PrimTy::Bool => self
                        .llvm_ctx
                        .bool_type()
                        .const_int(scalar.data, false)
                        .into(),
                    PrimTy::Int(kind) => self
                        .llvm_ctx
                        .custom_width_int_type(kind.bits() as u32)
                        .const_int(scalar.data, false)
                        .into(),
                    PrimTy::Float(kind) => match kind {
                        FloatKind::F32 => self
                            .llvm_ctx
                            .f32_type()
                            .const_float(scalar.data as f64)
                            .into(),
                        FloatKind::F64 => self
                            .llvm_ctx
                            .f64_type()
                            .const_float(scalar.data as f64)
                            .into(),
                    },
                    PrimTy::String => todo!(),
                },
                TyKind::Func(_, _) | TyKind::Unit => todo!(),
                TyKind::Existential(_) | TyKind::Forall(_, _) | TyKind::Error | TyKind::Var(_) => {
                    unreachable!()
                },
            },
            ConstKind::ZeroSized => self.conv_ty(const_.ty).const_zero(),
        }
    }

    //
    fn local_ptr(&self, local: Local) -> PointerValue<'ink> {
        self.locals_pointers.get(local).copied().unwrap()
    }

    // Types //
    fn conv_ty(&self, ty: Ty) -> BasicTypeEnum<'ink> {
        match ty.kind() {
            TyKind::Unit => self.llvm_ctx.i8_type().into(),
            TyKind::Prim(prim) => match prim {
                PrimTy::Bool => self.llvm_ctx.bool_type().into(),
                PrimTy::Int(kind) => self
                    .llvm_ctx
                    .custom_width_int_type(kind.bits().into())
                    .into(),
                PrimTy::Float(kind) => match kind {
                    FloatKind::F32 => self.llvm_ctx.f32_type().into(),
                    FloatKind::F64 => self.llvm_ctx.f64_type().into(),
                },
                PrimTy::String => todo!(),
            },
            TyKind::Func(param, body) => todo!(),
            TyKind::Func(_, _)
            | TyKind::Existential(_)
            | TyKind::Forall(_, _)
            | TyKind::Error
            | TyKind::Var(_) => {
                unreachable!()
            },
        }
    }
}
