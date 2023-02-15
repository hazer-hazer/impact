use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, BasicValueEnum},
};

use crate::{
    hir::{
        expr::{Expr, ExprKind, ExprNode, Lambda, Lit, PathExpr},
        item::{Decl, ItemKind, ItemNode},
        stmt::{Stmt, StmtKind, StmtNode},
        HirId, HIR,
    },
    message::message::{MessageHolder, MessageStorage},
    resolve::res::Res,
    session::Session,
    typeck::{
        ty::{FloatKind, IntKind},
        tyctx::TyCtx,
    },
};

pub struct Generator<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    // Values of resolutions
    res_values: HashMap<Res, BasicValueEnum<'ctx>>,

    sess: Session,
    msg: MessageStorage,
}

impl<'ctx> MessageHolder for Generator<'ctx> {
    fn save(&mut self, msg: crate::message::message::Message) {
        self.msg.add_message(msg)
    }
}

impl<'ctx> Generator<'ctx> {
    fn hir(&self) -> &HIR {
        &self.sess.hir
    }

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

pub trait NodeCodeGen<'g> {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g>;
}

impl<'g> NodeCodeGen<'g> for StmtNode {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.kind() {
            &StmtKind::Expr(expr) => g.hir().expr(expr).codegen(g),
            &StmtKind::Item(item) => g.hir().item(item).codegen(g),
        }
    }
}

impl<'g> NodeCodeGen<'g> for PathExpr {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        g.expect_res_value(g.hir().path(self.0).res())
    }
}

impl<'g> NodeCodeGen<'g> for Lambda {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let block = g.current_block();
        todo!()
    }
}

impl<'g> NodeCodeGen<'g> for ExprNode {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.kind() {
            ExprKind::Lit(lit) => match lit {
                Lit::Bool(val) => g.bool_value(*val),
                Lit::Int(val, kind) => g.int_value(*val, g.tyctx().tyof(self.id()).int_kind()),
                Lit::Float(val, kind) => {
                    g.float_value(*val, g.tyctx().tyof(self.id()).float_kind())
                },
                Lit::String(sym) => g.string_value(sym.as_str()),
            },
            ExprKind::Path(path) => path.codegen(g),
            ExprKind::Block(_) => todo!(),
            ExprKind::Lambda(_) => todo!(),
            ExprKind::Call(_) => todo!(),
            ExprKind::Let(_) => todo!(),
            ExprKind::Ty(_) => todo!(),
        }
    }
}

impl<'g> NodeCodeGen<'g> for ItemNode {
    fn codegen(&self, g: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.kind() {
            ItemKind::Decl(decl) => {
                todo!()
                // g.bind_res_value(Res::local(self), value)
            },
            ItemKind::Mod(_) | ItemKind::TyAlias(_) => {},
        }

        g.unit_value()
    }
}
