use std::{collections::HashMap};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, BasicTypeEnum, PointerType},
    values::{BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode},
    AddressSpace,
};

use crate::{
    hir::{HirId, Res, HIR},
    message::message::{MessageBuilder, MessageHolder, MessageStorage},
    resolve::def::DefId,
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Internable, Symbol},
    typeck::{
        ty::{FloatKind, IntKind, PrimTy, Ty, TyKind},
        tyctx::TyCtx,
    },
};

use super::nodes::NodeCodeGen;

pub struct CodeGen<'ctx> {
    pub hir: &'ctx HIR,

    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    /// Values of resolutions.
    /// We don't map resolved paths to values,
    ///  but different kinds of resolved items to their values,
    ///  i.e. in `a = 1` it is `Local(NodeId): 1`.
    res_values: HashMap<Res, BasicValueEnum<'ctx>>,

    // States //
    /// Name of current declaration, e.g. `a = 123`
    current_def: Option<(Ident, DefId)>,

    sess: Session,
    msg: MessageStorage,
}

impl<'ctx> MessageHolder for CodeGen<'ctx> {
    fn save(&mut self, msg: crate::message::message::Message) {
        self.msg.add_message(msg)
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(sess: Session, hir: &'ctx HIR, ctx: &'ctx Context) -> Self {
        Self {
            sess,
            hir,
            ctx,

            // FIXME
            module: ctx.create_module("kek"),
            builder: ctx.create_builder(),

            res_values: Default::default(),
            current_def: Default::default(),
            msg: MessageStorage::default(),
        }
    }

    pub(super) fn tyctx(&self) -> &TyCtx {
        &self.sess.tyctx
    }

    pub(super) fn builder(&mut self) -> &mut Builder<'ctx> {
        &mut self.builder
    }

    pub(super) fn current_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block().unwrap()
    }

    pub(super) fn under_def<T>(
        &mut self,
        def: (Ident, DefId),
        mut f: impl FnMut(&mut Self) -> T,
    ) -> T {
        self.current_def = Some(def);

        let res = f(self);

        self.current_def = None;

        res
    }

    // Definitions //
    pub(super) fn bind_res_value(&mut self, res: Res, value: BasicValueEnum<'ctx>) {
        assert!(self.res_values.insert(res, value).is_none());
    }

    pub(super) fn expect_res_value(&self, res: &Res) -> BasicValueEnum<'ctx> {
        *self
            .res_values
            .get(&res)
            .expect(format!("No value found for resolution {}", res).as_str())
    }

    // Types //
    pub(super) fn conv_ty(&self, ty: Ty) -> BasicTypeEnum<'ctx> {
        match ty.kind() {
            TyKind::Error => panic!("Error type at codegen stage"),
            TyKind::Unit => self.ctx.struct_type(&[], false).into(),
            TyKind::Prim(prim) => match prim {
                PrimTy::Bool => self.ctx.bool_type().into(),
                &PrimTy::Int(kind) => self.ctx.custom_width_int_type(self.int_bits(kind)).into(),
                PrimTy::Float(kind) => match kind {
                    FloatKind::F32 => self.ctx.f32_type().into(),
                    FloatKind::F64 => self.ctx.f64_type().into(),
                },
                PrimTy::String => todo!(),
            },
            TyKind::Var(_) => todo!(),
            TyKind::Existential(_) => todo!(),
            &TyKind::Func(param, body) => self.conv_func_ty((param, body)).into(),
            TyKind::Forall(_, _) => todo!(),
        }
    }

    pub(super) fn conv_func_ty(&self, func_ty: (Ty, Ty)) -> PointerType<'ctx> {
        let param = self.conv_ty(func_ty.0);
        let body = self.conv_ty(func_ty.1);
        body.fn_type(&[param.into()], false)
            .ptr_type(AddressSpace::default())
    }

    // Values //
    pub(super) fn bool_value(&self, val: bool) -> BasicValueEnum<'ctx> {
        self.ctx.bool_type().const_int(val as u64, true).into()
    }

    fn ptr_size() -> usize {
        std::mem::size_of::<*const u8>()
    }

    fn int_bits(&self, kind: IntKind) -> u32 {
        match kind {
            IntKind::U8 => 8,
            IntKind::U16 => 16,
            IntKind::U32 => 32,
            IntKind::U64 => 64,
            IntKind::I8 => 8,
            IntKind::I16 => 16,
            IntKind::I32 => 32,
            IntKind::I64 => 64,
            IntKind::Int | IntKind::Uint => Self::ptr_size() as u32 * 8,
        }
    }

    pub(super) fn int_value(&self, val: u64, kind: IntKind) -> BasicValueEnum<'ctx> {
        let bits = self.int_bits(kind);
        let unsigned = kind.is_unsigned();
        self.ctx
            .custom_width_int_type(bits)
            .const_int(val, unsigned)
            .as_basic_value_enum()
    }

    pub(super) fn float_value(&self, val: f64, kind: FloatKind) -> BasicValueEnum<'ctx> {
        match kind {
            FloatKind::F32 => self.ctx.f32_type().const_float(val).into(),
            FloatKind::F64 => self.ctx.f64_type().const_float(val).into(),
        }
    }

    pub(super) fn string_value(&self, str: &str) -> BasicValueEnum<'ctx> {
        let lit = self.ctx.const_string(str.as_bytes(), true);

        let global = self
            .module
            .add_global(lit.get_type(), None, "string_literal");

        global.set_initializer(&lit);

        let val = global.as_pointer_value();

        let cstring_ty = self.ctx.i8_type().ptr_type(AddressSpace::from(0));

        let cast = self
            .builder
            .build_pointer_cast(val, cstring_ty, "string_cast");

        cast.as_basic_value_enum()
    }

    pub(super) fn unit_value(&self) -> BasicValueEnum<'ctx> {
        // TODO: Compile to void for functions
        let unit = self.ctx.struct_type(&[], false);
        unit.const_zero().into()
    }

    pub(super) fn get_lambda_name(&self) -> Symbol {
        self.current_def
            .map_or("lambda".intern(), |(name, _)| name.sym())
    }

    pub(super) fn function(
        &self,
        name: &str,
        ty: Ty,
    ) -> (FunctionValue<'ctx>, BasicValueEnum<'ctx>) {
        let raw_ty = self
            .conv_func_ty(ty.as_func())
            .get_element_type()
            .into_function_type();

        let func = self
            .module
            .add_function(name, raw_ty, Some(Linkage::Internal));

        let func_ptr = func.as_global_value().as_pointer_value().into();

        // if let Some((_, def_id)) = self.current_def {
        //     self.res_values.
        // }

        let basic_block = self.ctx.append_basic_block(func, "entry");
        self.builder.position_at_end(basic_block);

        (func, func_ptr)
    }

    // Building //
    fn is_at_block_terminator(&self) -> bool {
        let instr = self.current_block().get_last_instruction();
        matches!(
            instr.map(|instr| instr.get_opcode()),
            Some(InstructionOpcode::Return | InstructionOpcode::Unreachable)
        )
    }

    pub(super) fn build_return(&mut self, ret_val: BasicValueEnum<'ctx>) {
        if !self.is_at_block_terminator() {
            self.builder.build_return(Some(&ret_val));
        }
    }

    fn build_main(&mut self) {
        // main function //
        // FIXME: There might be better place for this logic

        let main_func_def_id = match self.sess.def_table.main_func() {
            Ok(ok) => ok,
            Err(err) => {
                err.emit(self);
                return;
            },
        };

        // FIXME: Actually must be `() -> ()`
        let expected_main_func_ty = Ty::func(Ty::unit(), Ty::unit());

        if expected_main_func_ty != self.tyctx().tyof(HirId::new_owner(main_func_def_id)) {
            let span = self.sess.def_table.def_name_span(main_func_def_id);
            MessageBuilder::error()
                .span(span)
                .text(format!("'main' function has invalid type"))
                .label(span, format!("Must have type {}", expected_main_func_ty))
                .emit(self);
            return;
        }

        let i32_ty = self.ctx.i32_type();
        let main_func_ty = i32_ty.fn_type(&[], false);
        let main_func_val = self
            .module
            .add_function("main", main_func_ty, Some(Linkage::External));
        let basic_block = self.ctx.append_basic_block(main_func_val, "entry");

        self.builder.position_at_end(basic_block);

        self.hir.root().items.iter().for_each(|&item| {
            self.hir.item(item).codegen(self);
        });

        let success_ret = i32_ty.const_int(0, true);
        self.build_return(success_ret.into());
    }
}

impl<'ctx> Stage<Module<'ctx>> for CodeGen<'ctx> {
    fn run(mut self) -> StageOutput<Module<'ctx>> {
        self.build_main();
        StageOutput::new(self.sess, self.module, self.msg)
    }
}
