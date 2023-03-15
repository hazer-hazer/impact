use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    types::BasicTypeEnum,
    values::{BasicValueEnum, CallableValue, FunctionValue, PointerValue},
};

use crate::{
    dt::idx::IndexVec,
    hir::BodyId,
    mir::{
        scalar::Scalar, Body, Const, ConstKind, Local, Operand, RValue, Stmt, StmtKind, Terminator,
        TerminatorKind, Ty, BB, MIR, START_BB,
    },
    resolve::def::{DefId, DefMap},
    session::Session,
    typeck::ty::{FloatKind, PrimTy, TyKind},
};

use super::{codegen::CodeGen, ctx::CodeGenCtx, func::FunctionMap};

pub struct BodyCodeGen<'ink, 'ctx> {
    ctx: CodeGenCtx<'ink, 'ctx>,

    // Body context //
    func_def_id: DefId,
    body_id: BodyId,
    body: &'ctx Body,
    func: FunctionValue<'ink>,
    builder: Builder<'ink>,
    /// Mapping locals to alloca pointers. Created as a decent IndexVec for optimization reasons and avoiding IndexVec<_, Option<_>>.
    locals_pointers: IndexVec<Local, PointerValue<'ink>>,
    function_map: &'ink FunctionMap<'ink>,
}

impl<'ink, 'ctx> BodyCodeGen<'ink, 'ctx> {
    pub fn new(
        ctx: CodeGenCtx<'ink, 'ctx>,
        func_def_id: DefId,
        function_map: &'ink FunctionMap<'ink>,
    ) -> Self {
        let func = function_map.expect(func_def_id);
        let body_id = ctx.hir.owner_body(func_def_id.into()).unwrap();
        let body = ctx.mir.expect(body_id);

        let builder = ctx.llvm_ctx.create_builder();

        let body_bb = ctx.llvm_ctx.append_basic_block(func, "body");
        builder.position_at_end(body_bb);

        Self {
            ctx,
            func_def_id,
            body_id,
            body,
            func,
            builder,
            locals_pointers: IndexVec::new_of(body.locals.len()),
            function_map,
        }
    }

    fn new_alloca_builder(&self) -> Builder<'ink> {
        let alloca_builder = self.ctx.llvm_ctx.create_builder();
        let first_bb = self.func.get_first_basic_block().unwrap();

        if let Some(first_inst) = first_bb.get_first_instruction() {
            alloca_builder.position_before(&first_inst);
        } else {
            alloca_builder.position_at_end(first_bb);
        }

        alloca_builder
    }

    // pub fn new(
    //     sess: &'ctx Session,
    //     mir: &'ctx MIR,
    //     body_id: BodyId,
    //     llvm_ctx: &'ink Context,
    //     func: FunctionValue<'ink>,
    //     function_map: DefMap<FunctionValue<'ink>>,
    // ) -> Self {
    //     let builder = llvm_ctx.create_builder();

    //     let body_bb = llvm_ctx.append_basic_block(func, "body");
    //     builder.position_at_end(body_bb);

    //     Self {
    //         body_id,
    //         body: mir.expect(body_id),
    //         builder,
    //         func,
    //         locals_pointers: Default::default(),
    //     }
    // }

    // Generators //
    pub fn gen_body(&mut self) {
        self.body.params().for_each(|(local, _)| {
            let alloca_builder = self.new_alloca_builder();
            let func_param = self.func.get_nth_param(local.inner()).unwrap();
            let param_ptr = alloca_builder.build_alloca(func_param.get_type(), &local.to_string());
            alloca_builder.build_store(param_ptr, func_param);
        });

        let ll_bb = self.gen_bb(START_BB);
    }

    fn gen_bb(&mut self, bb: BB) -> BasicBlock<'ink> {
        let ll_bb = self
            .ctx
            .llvm_ctx
            .append_basic_block(self.func, &format!("bb{}", bb.inner()));
        let bb = self.body.bb(bb);

        self.builder.position_at_end(ll_bb);

        bb.stmts.iter().for_each(|stmt| self.gen_stmt(stmt));
        self.gen_terminator(&bb.terminator);

        ll_bb
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

    fn gen_terminator(&mut self, terminator: &Terminator) {
        match terminator.kind {
            TerminatorKind::Goto(bb) => {
                let ll_bb = self.gen_bb(bb);
                self.builder.build_unconditional_branch(ll_bb);
            },
            TerminatorKind::Return => {
                let return_local = self
                    .builder
                    .build_load(self.local_ptr(Local::return_local()), "load_return_load");
                self.builder.build_return(Some(&return_local));
            },
        }
    }

    // To-value converters //
    fn unit_value(&mut self) -> BasicValueEnum<'ink> {
        self.ctx.llvm_ctx.struct_type(&[], false).into()
    }

    fn rvalue_to_value(&mut self, rvalue: &RValue) -> BasicValueEnum<'ink> {
        match rvalue {
            RValue::Operand(operand) => self.operand_to_value(operand),
            RValue::Infix(_, _, _) => todo!(),
            &RValue::Closure(def_id) => self
                .function_map
                .expect(def_id)
                .as_global_value()
                .as_pointer_value()
                .into(),
            RValue::Def(_, _) => todo!(),
            RValue::Call { lhs, arg } => {
                let func = CallableValue::try_from(self.operand_to_value(lhs).into_pointer_value())
                    .unwrap();
                let arg = self.operand_to_value(arg).into();
                self.builder
                    .build_call(func, &[arg], "call")
                    .try_as_basic_value()
                    .unwrap_left()
            },
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

    // fn operand_to_func_value(&mut self, operand: &Operand) -> FunctionValue<'ink> {
    //     match operand {
    //         Operand::LValue(lv) => {
    //             self.builder.build_load(self.local_ptr(lv.local), "load_func")
    //         },
    //         Operand::Const(_) => todo!(),
    //     }
    // }

    fn const_to_value(&mut self, const_: &Const) -> BasicValueEnum<'ink> {
        match const_.kind {
            ConstKind::Scalar(scalar) => match const_.ty.kind() {
                TyKind::Prim(prim) => match prim {
                    PrimTy::Bool => self
                        .ctx
                        .llvm_ctx
                        .bool_type()
                        .const_int(scalar.data, false)
                        .into(),
                    PrimTy::Int(kind) => self
                        .ctx
                        .llvm_ctx
                        .custom_width_int_type(kind.bits() as u32)
                        .const_int(scalar.data, false)
                        .into(),
                    PrimTy::Float(kind) => match kind {
                        FloatKind::F32 => self
                            .ctx
                            .llvm_ctx
                            .f32_type()
                            .const_float(scalar.data as f64)
                            .into(),
                        FloatKind::F64 => self
                            .ctx
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
            ConstKind::ZeroSized => self.ctx.conv_basic_ty(const_.ty).const_zero(),
        }
    }

    //
    fn local_ptr(&self, local: Local) -> PointerValue<'ink> {
        self.locals_pointers.get(local).copied().unwrap()
    }
}
