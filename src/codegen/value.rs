use inkwell::values::BasicValueEnum;

use crate::{
    hir::visitor::HirVisitor,
    resolve::def::{DefId, DefMap},
};

use super::{ctx::CodeGenCtx, func::FunctionMap};

#[derive(Default)]
pub struct ValueMap<'ink>(DefMap<BasicValueEnum<'ink>>);

impl<'ink> ValueMap<'ink> {
    pub fn expect(&self, def_id: DefId) -> BasicValueEnum<'ink> {
        self.0.get_copied_unwrap(def_id)
    }

    fn insert(&mut self, def_id: DefId, value: BasicValueEnum<'ink>) {
        assert!(self.0.insert(def_id, value).is_none());
    }
}

pub struct ValueCodeGen<'ink, 'ctx, 'a> {
    ctx: CodeGenCtx<'ink, 'ctx>,

    function_map: &'a FunctionMap<'ink>,
    value_map: ValueMap<'ink>,
}

impl<'ink, 'ctx, 'a> ValueCodeGen<'ink, 'ctx, 'a> {
    pub fn new(ctx: CodeGenCtx<'ink, 'ctx>, function_map: &'a FunctionMap<'ink>) -> Self {
        Self {
            ctx,
            function_map,
            value_map: Default::default(),
        }
    }

    pub fn gen_values(mut self) -> ValueMap<'ink> {
        self.visit_hir(self.ctx.hir);

        self.value_map
    }
}

impl<'ink, 'ctx, 'a> HirVisitor for ValueCodeGen<'ink, 'ctx, 'a> {
    fn visit_value_item(
        &mut self,
        name: crate::span::span::Ident,
        value: &crate::hir::BodyId,
        id: crate::hir::item::ItemId,
        hir: &crate::hir::HIR,
    ) {
        let builder = self.ctx.llvm_ctx.create_builder();
        self.value_map.insert(
            id.def_id(),
            builder
                .build_call(
                    self.function_map.expect_mono(id.def_id()),
                    &[self.ctx.unit_value().into()],
                    &format!("{}_val_init", name),
                )
                .try_as_basic_value()
                .unwrap_left(),
        );
    }
}
