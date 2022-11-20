use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, BasicValueEnum},
};

use crate::{
    hir::expr::{Expr, ExprKind, Lit, PathExpr},
    resolve::res::Res,
    typeck::ty::{FloatKind, IntKind},
};

pub struct Generator<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    // Values of resolutions
    res_values: HashMap<Res, BasicValueEnum<'ctx>>,
}

impl<'ctx> Generator<'ctx> {
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

        let cstring_ty = self.ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic);

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
}

pub trait CodeGen<'g> {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g>;
}

impl<'g> CodeGen<'g> for Lit {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self {
            Lit::Bool(val) => g.bool_value(*val),
            Lit::Int(val, kind) => g.int_value(*val, *kind),
            Lit::Float(val, kind) => g.float_value(*val, *kind),
            Lit::String(sym) => g.string_value(sym.as_str()),
        }
    }
}

impl<'g> CodeGen<'g> for PathExpr {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        g.expect_res_value(self.0.res())
    }
}

impl<'g> CodeGen<'g> for Expr {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.kind() {
            ExprKind::Unit => g.unit_value(),
            ExprKind::Lit(lit) => lit.codegen(g),
            ExprKind::Path(path) => path.codegen(g),
            ExprKind::Block(_) => todo!(),
            ExprKind::Lambda(_) => todo!(),
            ExprKind::Call(_) => todo!(),
            ExprKind::Let(_) => todo!(),
            ExprKind::Ty(_) => todo!(),
        }
    }
}
