use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, PointerType},
    values::{BasicValue, BasicValueEnum, CallableValue, FunctionValue, InstructionOpcode},
    AddressSpace,
};

use crate::{
    hir::{
        expr::{BlockNode, Call, ExprKind, ExprNode, Lambda, Lit, PathExpr},
        item::{ItemKind, ItemNode},
        stmt::{StmtKind, StmtNode},
        Path, PathNode, Res, HIR,
    },
    message::message::{MessageHolder, MessageStorage},
    resolve::def::DefId,
    session::Session,
    span::span::{Ident, Internable},
    typeck::{
        ty::{FloatKind, IntKind, PrimTy, Ty, TyKind},
        tyctx::TyCtx,
    },
};

pub struct Generator<'ctx> {
    hir: &'ctx HIR,

    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    // Values of resolutions
    res_values: HashMap<Res, BasicValueEnum<'ctx>>,

    // States //
    /// Name of current declaration, e.g. `a = 123`
    current_def: Option<(Ident, DefId)>,

    sess: Session,
    msg: MessageStorage,
}

impl<'ctx> MessageHolder for Generator<'ctx> {
    fn save(&mut self, msg: crate::message::message::Message) {
        self.msg.add_message(msg)
    }
}

impl<'ctx> Generator<'ctx> {
    fn tyctx(&self) -> &TyCtx {
        &self.sess.tyctx
    }

    fn current_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block().unwrap()
    }

    // Definitions //
    fn bind_res_value(&mut self, res: Res, value: BasicValueEnum<'ctx>) {
        self.res_values.insert(res, value);
    }

    fn expect_res_value(&self, res: &Res) -> BasicValueEnum<'ctx> {
        *self
            .res_values
            .get(&res)
            .expect(format!("No value found for resolution {}", res).as_str())
    }

    // Types //
    fn conv_ty(&self, ty: Ty) -> BasicTypeEnum<'ctx> {
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

    fn conv_func_ty(&self, func_ty: (Ty, Ty)) -> PointerType<'ctx> {
        let param = self.conv_ty(func_ty.0);
        let body = self.conv_ty(func_ty.1);
        body.fn_type(&[param.into()], false)
            .ptr_type(AddressSpace::default())
    }

    // Values //
    fn bool_value(&self, val: bool) -> BasicValueEnum<'ctx> {
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

    fn int_value(&self, val: u64, kind: IntKind) -> BasicValueEnum<'ctx> {
        let bits = self.int_bits(kind);
        let unsigned = kind.is_unsigned();
        self.ctx
            .custom_width_int_type(bits)
            .const_int(val, unsigned)
            .as_basic_value_enum()
    }

    fn float_value(&self, val: f64, kind: FloatKind) -> BasicValueEnum<'ctx> {
        match kind {
            FloatKind::F32 => self.ctx.f32_type().const_float(val).into(),
            FloatKind::F64 => self.ctx.f64_type().const_float(val).into(),
        }
    }

    fn string_value(&self, str: &str) -> BasicValueEnum<'ctx> {
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

    fn unit_value(&self) -> BasicValueEnum<'ctx> {
        // TODO: Compile to void for functions
        let unit = self.ctx.struct_type(&[], false);
        unit.const_zero().into()
    }

    fn function(&self, name: &str, ty: Ty) -> (FunctionValue<'ctx>, BasicValueEnum<'ctx>) {
        let raw_ty = self
            .conv_func_ty(ty.as_func())
            .get_element_type()
            .into_function_type();

        let func = self
            .module
            .add_function(name, raw_ty, Some(inkwell::module::Linkage::Internal));

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

    fn build_return(&mut self, ret_val: BasicValueEnum<'ctx>) {
        if !self.is_at_block_terminator() {
            self.builder.build_return(Some(&ret_val));
        }
    }
}

pub trait NodeCodeGen<'g> {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g>;
}

impl<'g> NodeCodeGen<'g> for StmtNode {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.kind() {
            &StmtKind::Expr(expr) => g.hir.expr(expr).codegen(g),
            &StmtKind::Item(item) => g.hir.item(item).codegen(g),
        }
    }
}

impl<'g> NodeCodeGen<'g> for PathNode {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        g.expect_res_value(self.res())
    }
}

impl<'g> NodeCodeGen<'g> for ExprNode {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.kind() {
            ExprKind::Lit(lit) => match lit {
                Lit::Bool(val) => g.bool_value(*val),

                // FIXME: Assert int/float kinds eq to types
                Lit::Int(val, _kind) => g.int_value(*val, g.tyctx().tyof(self.id()).as_int_kind()),
                Lit::Float(val, _kind) => {
                    g.float_value(*val, g.tyctx().tyof(self.id()).as_float_kind())
                },

                Lit::String(sym) => g.string_value(sym.as_str()),
            },
            &ExprKind::Path(PathExpr(path)) => g.hir.path(path).codegen(g),
            ExprKind::Block(_) => todo!(),
            &ExprKind::Lambda(Lambda { param, body }) => {
                let caller_block = g.current_block();
                let name = g
                    .current_def
                    .map_or("lambda".intern(), |(name, _)| name.sym());

                let (func, func_val) = g.function(name.as_str(), g.tyctx().tyof(self.id()));

                g.bind_res_value(Res::local(param), func.get_nth_param(0).unwrap());

                let ret_val = g.hir.expr(body).codegen(g);

                g.build_return(ret_val);
                g.builder.position_at_end(caller_block);

                func_val
            },
            &ExprKind::Call(Call { lhs, arg }) => {
                let func = g.hir.expr(lhs).codegen(g).into_pointer_value();
                let arg = g.hir.expr(arg).codegen(g);

                let func = CallableValue::try_from(func).unwrap();
                g.builder
                    .build_call(func, &[arg.into()], "")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            },
            ExprKind::Let(_) => todo!(),
            ExprKind::Ty(_) => todo!(),
        }
    }
}

impl<'g> NodeCodeGen<'g> for BlockNode {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        assert!(!self.stmts().is_empty() || self.expr().is_some());

        self.stmts().iter().for_each(|&stmt| {
            g.hir.stmt(stmt).codegen(g);
        });

        if let Some(&expr) = self.expr() {
            g.hir.expr(expr).codegen(g)
        } else {
            g.unit_value()
        }
    }
}

impl<'g> NodeCodeGen<'g> for ItemNode {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.kind() {
            ItemKind::Decl(_decl) => {
                g.current_def = Some((self.name(), self.def_id()));

                assert!(g.res_values.contains_key(self.def_id()))

                g.current_def = None
            },
            ItemKind::Mod(_) | ItemKind::TyAlias(_) => {},
        }

        g.unit_value()
    }
}
