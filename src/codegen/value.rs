use inkwell::values::BasicValueEnum;

use super::{ctx::CodeGenCtx, func::FunctionMap};
use crate::{
    hir::{visitor::HirVisitor, HIR},
    message::message::{impl_message_holder, MessageStorage},
    resolve::def::{DefId, DefMap},
    session::{impl_session_holder, stage_result, SessionHolder, Stage, StageResult},
};

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
    msg: MessageStorage,
}

impl_message_holder!(ValueCodeGen<'ink, 'ctx, 'a>);
impl_session_holder!(ValueCodeGen<'ink, 'ctx, 'a>; ctx.sess);

impl<'ink, 'ctx, 'a> Stage<ValueMap<'ink>, CodeGenCtx<'ink, 'ctx>>
    for ValueCodeGen<'ink, 'ctx, 'a>
{
    fn run(self) -> StageResult<ValueMap<'ink>, CodeGenCtx<'ink, 'ctx>> {
        stage_result(self.ctx, self.value_map, self.msg)
    }
}

impl<'ink, 'ctx, 'a> ValueCodeGen<'ink, 'ctx, 'a> {
    pub fn new(ctx: CodeGenCtx<'ink, 'ctx>, function_map: &'a FunctionMap<'ink>) -> Self {
        Self {
            ctx,
            function_map,
            value_map: Default::default(),
            msg: Default::default(),
        }
    }

    fn gen_values(mut self) -> ValueMap<'ink> {
        self.visit_hir(self.ctx.hir);

        self.value_map
    }
}

impl<'ink, 'ctx, 'a> HirVisitor for ValueCodeGen<'ink, 'ctx, 'a> {
    fn visit_value_item(
        &mut self,
        _name: crate::span::sym::Ident,
        _value: crate::hir::BodyId,
        _id: crate::hir::item::ItemId,
        _hir: &HIR,
    ) {
        // let builder = self.ctx.llvm_ctx.create_builder();
        // self.value_map.insert(
        //     id.def_id(),
        //     builder
        //         .build_call(
        //             self.function_map.expect_mono(id.def_id()),
        //             &[self.ctx.unit_value().into()],
        //             &format!("{}_val_init", name),
        //         )
        //         .try_as_basic_value()
        //         .unwrap_left(),
        // );
    }
}
