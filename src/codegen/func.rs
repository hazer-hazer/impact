use inkwell::values::FunctionValue;

use crate::{
    hir::{item::ItemId, visitor::HirVisitor, BodyId, BodyOwner, HIR},
    resolve::def::{DefId, DefMap},
};

use super::ctx::CodeGenCtx;

pub struct FunctionMap<'ink>(DefMap<FunctionValue<'ink>>);

impl<'ink> FunctionMap<'ink> {
    pub fn expect(&self, def_id: DefId) -> FunctionValue<'ink> {
        self.0.get_copied_unwrap(def_id)
    }

    fn insert(&mut self, def_id: DefId, value: FunctionValue<'ink>) {
        assert!(self.0.insert(def_id, value).is_none());
    }
}

pub struct FunctionsCodeGen<'ink, 'ctx> {
    ctx: CodeGenCtx<'ink, 'ctx>,

    function_map: FunctionMap<'ink>,
}

impl<'ink, 'ctx> HirVisitor for FunctionsCodeGen<'ink, 'ctx> {
    fn visit_body(&mut self, &body: &BodyId, owner: BodyOwner, hir: &HIR) {
        let def_id = owner.def_id;
        let func_ty = self.ctx.func_ty(def_id);

        let func_value = self.ctx.llvm_module.add_function(
            self.ctx
                .hir
                .item_name(ItemId::new(def_id.into()))
                .sym()
                .as_str(),
            func_ty,
            Some(inkwell::module::Linkage::External),
        );

        self.function_map.insert(def_id, func_value);
    }
}

impl<'ink, 'ctx> FunctionsCodeGen<'ink, 'ctx> {
    pub fn gen_functions(mut self) -> FunctionMap<'ink> {
        self.visit_hir(self.ctx.hir);
        self.function_map
    }
}
