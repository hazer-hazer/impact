use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    values::{BasicValue, BasicValueEnum, CallableValue, FunctionValue, PointerValue},
};

use crate::{
    cli::verbose,
    dt::idx::IndexVec,
    hir::BodyId,
    mir::{
        Body, Const, ConstKind, Local, Operand, RValue, Stmt, StmtKind, Terminator, TerminatorKind,
        Ty, BB, START_BB,
    },
    resolve::def::DefId,
    typeck::ty::{FloatKind, TyKind},
};

use super::{ctx::CodeGenCtx, func::FunctionMap, value::ValueMap};

pub struct BodyCodeGen<'ink, 'ctx, 'a> {
    ctx: CodeGenCtx<'ink, 'ctx>,

    // Body context //
    func_def_id: DefId,
    body_id: BodyId,
    body: &'ctx Body,
    func_ty: Ty,
    func: FunctionValue<'ink>,
    builder: Builder<'ink>,
    /// Mapping locals to alloca pointers. Created as a decent IndexVec for optimization reasons and avoiding IndexVec<_, Option<_>>.
    locals_values: IndexVec<Local, Option<PointerValue<'ink>>>,
    function_map: &'a FunctionMap<'ink>,
    value_map: &'a ValueMap<'ink>,
}

impl<'ink, 'ctx, 'a> BodyCodeGen<'ink, 'ctx, 'a> {
    pub fn new(
        ctx: CodeGenCtx<'ink, 'ctx>,
        func_def_id: DefId,
        func_ty: Ty,
        func: FunctionValue<'ink>,
        function_map: &'a FunctionMap<'ink>,
        value_map: &'a ValueMap<'ink>,
    ) -> Self {
        let body_id = ctx.hir.owner_body(func_def_id.into()).unwrap();
        let body = ctx.mir.expect(body_id);

        let builder = ctx.llvm_ctx.create_builder();

        Self {
            ctx,
            func_def_id,
            body_id,
            body,
            func_ty,
            func,
            builder,
            locals_values: IndexVec::new_of(body.locals.len()),
            function_map,
            value_map,
        }
    }

    fn new_alloca_builder(&self) -> Builder<'ink> {
        let first_bb = self.func.get_first_basic_block().unwrap();
        let alloca_builder = self.ctx.llvm_ctx.create_builder();

        if let Some(first_instr) = first_bb.get_first_instruction() {
            alloca_builder.position_before(&first_instr);
        } else {
            alloca_builder.position_at_end(first_bb);
        }

        alloca_builder
    }

    // Generators //
    pub fn gen_body(&mut self) {
        verbose!("Gen body of function{}: {}", self.func_def_id, self.func_ty);

        let body_bb = self.ctx.llvm_ctx.append_basic_block(self.func, "body");
        self.builder.position_at_end(body_bb);

        let alloca_builder = self.new_alloca_builder();
        let return_local_ty = self.ctx.conv_basic_ty(self.func_ty.return_ty());
        let return_local_value = alloca_builder.build_alloca(return_local_ty, "alloca");
        self.add_local(Local::return_local(), return_local_value);

        self.body.params().for_each(|(local, _)| {
            let func_param = self.func.get_nth_param(local.inner()).unwrap();
            let value = alloca_builder.build_alloca(func_param.get_type(), &local.formatted());
            alloca_builder.build_store(value, func_param);
            self.add_local(local, value);
        });

        self.body.inner_locals().for_each(|(local, info)| {
            // TODO: Add name to `LocalInfo` and use it here
            let value =
                alloca_builder.build_alloca(self.ctx.conv_basic_ty(info.ty), &local.formatted());
            self.add_local(local, value);
        });

        let start_bb = self.gen_bb(START_BB);

        self.builder.position_at_end(body_bb);
        self.builder.build_unconditional_branch(start_bb);
    }

    fn gen_bb(&mut self, bb: BB) -> BasicBlock<'ink> {
        verbose!("Gen bb {}", bb);

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
        verbose!("Gen stmt {}", stmt);

        match &stmt.kind {
            StmtKind::Assign(lv, rv) => {
                let ptr = self.local_value(lv.local);
                let value = self.rvalue_to_value(rv);
                self.builder.build_store(ptr, value);
            },
        }
    }

    fn gen_terminator(&mut self, terminator: &Terminator) {
        verbose!("Gen terminator {}", terminator);

        match terminator.kind {
            TerminatorKind::Goto(bb) => {
                let ll_bb = self.gen_bb(bb);
                self.builder.build_unconditional_branch(ll_bb);
            },
            TerminatorKind::Return => {
                let return_local = self
                    .builder
                    .build_load(self.local_value(Local::return_local()), "load_return_load");
                self.builder.build_return(Some(&return_local));
            },
        }
    }

    // To-value converters //
    fn rvalue_to_value(&mut self, rvalue: &RValue) -> BasicValueEnum<'ink> {
        verbose!("Convert RValue to BasicValue {}", rvalue);

        match rvalue {
            RValue::Operand(operand) => self.operand_to_value(operand),
            &RValue::Infix(op) => self
                .function_map
                .infix(op)
                .as_global_value()
                .as_pointer_value()
                .into(),
            &RValue::Closure(def_id) => self
                .function_map
                .expect_mono(def_id)
                .as_global_value()
                .as_pointer_value()
                .into(),
            &RValue::FuncRef(def_id, ty) => {
                assert!(ty.is_func_like());
                assert!(ty.is_instantiated());

                self.function_map
                    .instance(def_id, ty)
                    .as_global_value()
                    .as_pointer_value()
                    .into()
            },
            &RValue::ClosureRef(def_id) => self
                .function_map
                .expect_mono(def_id)
                .as_global_value()
                .as_pointer_value()
                .into(),
            &RValue::ValueRef(def_id) => self
                .builder
                .build_call(
                    self.function_map.expect_mono(def_id),
                    &[self.ctx.unit_value().into()],
                    &format!(
                        "{}_val_init",
                        self.ctx.sess.def_table.get_def(def_id).name()
                    ),
                )
                .try_as_basic_value()
                .unwrap_left(),
            RValue::Call { lhs, args } => {
                let func = CallableValue::try_from(self.operand_to_value(lhs).into_pointer_value())
                    .unwrap();
                let args = args
                    .iter()
                    .map(|arg| self.operand_to_value(arg).into())
                    .collect::<Vec<_>>();
                self.builder
                    .build_call(func, &args, "call")
                    .try_as_basic_value()
                    .unwrap_left()
            },
            RValue::Ref(lv) => self.local_value(lv.local).into(),
        }
    }

    fn operand_to_value(&mut self, operand: &Operand) -> BasicValueEnum<'ink> {
        verbose!("Convert operand {} to BasicValue", operand);

        match operand {
            Operand::LValue(lv) => {
                // TODO: Useful info in name
                self.builder.build_load(self.local_value(lv.local), "load")
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
        verbose!("Convert const {} to BasicValue", const_);

        match &const_.kind {
            ConstKind::Scalar(scalar) => match const_.ty.kind() {
                TyKind::Bool => self
                    .ctx
                    .llvm_ctx
                    .bool_type()
                    .const_int(scalar.data, false)
                    .into(),
                TyKind::Int(kind) => self
                    .ctx
                    .llvm_ctx
                    .custom_width_int_type(kind.bits())
                    .const_int(scalar.data, false)
                    .into(),

                TyKind::Float(kind) => match kind {
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
                TyKind::Str => todo!(),
                TyKind::Func(..) | TyKind::FuncDef(..) | TyKind::Unit => todo!(),
                TyKind::Ref(_)
                | TyKind::Existential(_)
                | TyKind::Forall(_, _)
                | TyKind::Error
                | TyKind::Var(_) => {
                    unreachable!()
                },
                TyKind::Kind(_) => todo!(),
            },
            ConstKind::ZeroSized => match const_.ty.kind() {
                TyKind::Unit => self.ctx.unit_value(),
                TyKind::Bool => todo!(),
                TyKind::Int(_) => todo!(),
                TyKind::Float(_) => todo!(),
                TyKind::Str => todo!(),
                TyKind::FuncDef(..) => todo!(),
                TyKind::Func(..) => todo!(),
                TyKind::Ref(_) => todo!(),
                TyKind::Error | TyKind::Var(_) | TyKind::Existential(_) | TyKind::Forall(_, _) => {
                    unreachable!()
                },
                TyKind::Kind(_) => todo!(),
            },
            ConstKind::Slice { data } => self.build_cstring_value(data),
        }
    }

    pub fn build_cstring_value(&self, bytes: &[u8]) -> BasicValueEnum<'ink> {
        let ptr = self.ctx.cstring_ptr_value(bytes);
        let cast =
            self.builder
                .build_pointer_cast(ptr, self.ctx.cstring_ptr_ty(), "string_slice_cast");
        cast.as_basic_value_enum()
    }

    // Locals //
    fn add_local(&mut self, local: Local, value: PointerValue<'ink>) {
        assert!(
            self.locals_values.insert(local, value).is_none(),
            "Duplicate local {} value insertion",
            local
        );
    }

    fn local_value(&self, local: Local) -> PointerValue<'ink> {
        self.locals_values.get_copied_unwrap(local)
    }
}
