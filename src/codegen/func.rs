use std::collections::HashMap;

use inkwell::{module::Module, types::FunctionType, values::FunctionValue};

use crate::{
    dt::idx::IndexVec,
    hir::{expr::Expr, item::ItemId, visitor::HirVisitor, BodyId, BodyOwner, HirMap, HIR},
    mir::Ty,
    resolve::def::{DefId, DefMap},
    typeck::{ty::TyMap, tyctx::InstantiatedTy},
    utils::macros::match_expected,
};

use super::ctx::CodeGenCtx;

pub enum FuncInstance<F> {
    Mono(F),
    Poly(HirMap<F>),
}

impl<F> FuncInstance<F> {
    fn add_poly(&mut self, expr: Expr, f: F) {
        match_expected!(self, Self::Poly(poly) => assert!(poly.insert(expr, f).is_none()));
    }
}

#[derive(Default)]
pub struct FunctionMap<'ink>(DefMap<FuncInstance<FunctionValue<'ink>>>);

impl<'ink> FunctionMap<'ink> {
    pub fn expect_mono(&self, def_id: DefId) -> &FunctionValue<'ink> {
        match self.0.get_unwrap(def_id) {
            FuncInstance::Mono(mono) => mono,
            FuncInstance::Poly(_) => panic!(),
        }
    }

    fn insert_mono(&mut self, ty: Ty, value: FunctionValue<'ink>) {
        assert!(self
            .0
            .insert(ty.func_def_id().unwrap(), FuncInstance::Mono(value))
            .is_none());
    }

    fn insert_poly(&mut self, expr: Expr, ty: Ty, value: FunctionValue<'ink>) {
        self.0
            .upsert(ty.func_def_id().unwrap(), || {
                FuncInstance::Poly(Default::default())
            })
            .add_poly(expr, value);
    }
}

pub struct FunctionsCodeGen<'ink, 'ctx> {
    ctx: CodeGenCtx<'ink, 'ctx>,
    llvm_module: Module<'ink>,

    function_map: FunctionMap<'ink>,
}

impl<'ink, 'ctx> HirVisitor for FunctionsCodeGen<'ink, 'ctx> {
    fn visit_body(&mut self, &_body: &BodyId, owner: BodyOwner, _hir: &HIR) {
        let def_id = owner.def_id;
        let func_ty = self.ctx.func_ty(def_id);

        match func_ty {
            InstantiatedTy::Mono((ty, ll_ty)) => {
                let value = self.func_value(def_id, ll_ty);
                self.function_map.insert_mono(ty, value);
            },
            InstantiatedTy::Poly(poly) => {
                poly.iter().for_each(|inst| {
                    let ((ty, ll_ty), expr) = inst.unwrap();
                    let value = self.func_value(def_id, ll_ty);
                    self.function_map.insert_poly(expr, ty, value);
                });
            },
        };
    }
}

impl<'ink, 'ctx> FunctionsCodeGen<'ink, 'ctx> {
    pub fn new(ctx: CodeGenCtx<'ink, 'ctx>) -> Self {
        Self {
            ctx,
            llvm_module: ctx.llvm_ctx.create_module("kek"),
            function_map: Default::default(),
        }
    }

    pub fn gen_functions(mut self) -> FunctionMap<'ink> {
        self.visit_hir(self.ctx.hir);

        self.function_map
    }

    fn func_value(&mut self, def_id: DefId, func_ty: FunctionType<'ink>) -> FunctionValue<'ink> {
        self.llvm_module.add_function(
            self.ctx
                .hir
                .item_name(ItemId::new(def_id.into()))
                .sym()
                .as_str(),
            func_ty,
            Some(inkwell::module::Linkage::External),
        )
    }
}
