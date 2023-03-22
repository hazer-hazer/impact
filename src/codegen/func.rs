use std::collections::HashMap;

use inkwell::{module::Module, targets::TargetMachine, types::FunctionType, values::FunctionValue};

use crate::{
    dt::idx::IndexVec,
    hir::{
        expr::Expr, item::ItemId, visitor::HirVisitor, BodyId, BodyOwner, BodyOwnerKind, HirId,
        HirMap, HIR,
    },
    mir::Ty,
    resolve::def::{DefId, DefKind, DefMap},
    typeck::{ty::TyMap, tyctx::InstantiatedTy},
    utils::macros::match_expected,
};

use super::ctx::CodeGenCtx;

// TODO: Rename `FunctionsCodeGen` to `BodyOwnersCodeGen`?

pub enum FuncInstance<'ink> {
    Mono(Ty, FunctionValue<'ink>),
    Poly(TyMap<(Ty, FunctionValue<'ink>)>),
}

impl<'ink> FuncInstance<'ink> {
    fn add_poly(&mut self, ty: Ty, value: FunctionValue<'ink>) {
        match_expected!(self, Self::Poly(poly) => assert!(poly.insert(ty.id(), (ty, value)).is_none()));
    }
}

#[derive(Default)]
pub struct FunctionMap<'ink>(DefMap<FuncInstance<'ink>>);

impl<'ink> FunctionMap<'ink> {
    pub fn expect_mono(&self, def_id: DefId) -> FunctionValue<'ink> {
        match self.0.get_unwrap(def_id) {
            &FuncInstance::Mono(_, mono) => mono,
            FuncInstance::Poly(_) => panic!(),
        }
    }

    pub fn instance(&self, def_id: DefId, ty: Ty) -> FunctionValue<'ink> {
        assert!(ty.is_instantiated());
        match self.0.get_unwrap(def_id) {
            &FuncInstance::Mono(_, mono) => mono,
            FuncInstance::Poly(poly) => poly.get_copied_unwrap(ty.id()).1,
        }
    }

    fn insert_mono(&mut self, ty: Ty, value: FunctionValue<'ink>) {
        assert!(ty.is_instantiated());
        assert!(self
            .0
            .insert(ty.func_def_id().unwrap(), FuncInstance::Mono(ty, value))
            .is_none());
    }

    fn insert_poly(&mut self, ty: Ty, value: FunctionValue<'ink>) {
        assert!(ty.is_instantiated());
        self.0
            .upsert(ty.func_def_id().unwrap(), || {
                FuncInstance::Poly(Default::default())
            })
            .add_poly(ty, value);
    }

    pub fn iter(&self) -> impl Iterator<Item = (DefId, &FuncInstance<'ink>)> + '_ {
        self.0.iter_enumerated_flat()
    }
}

pub struct FunctionsCodeGen<'ink, 'ctx> {
    ctx: CodeGenCtx<'ink, 'ctx>,
    llvm_module: &'ctx Module<'ink>,

    function_map: FunctionMap<'ink>,
}

impl<'ink, 'ctx> HirVisitor for FunctionsCodeGen<'ink, 'ctx> {
    fn visit_body(&mut self, &_body: &BodyId, owner: BodyOwner, _hir: &HIR) {
        let def_id = owner.def_id;

        // Do not codegen `builtin` func
        if matches!(
            self.ctx.sess.def_table.get_def(def_id).kind(),
            DefKind::DeclareBuiltin
        ) {
            return;
        }

        // For Func or Lambda owner we get its type (possibly polymorphic, in which case we monomorphize it).
        // For value we'll generate anonymous function of type `() -> ValueType` and then immediately call it.
        let func_ty = match owner.kind {
            BodyOwnerKind::Value => InstantiatedTy::Mono(Ty::func(
                None,
                Ty::unit(),
                self.ctx.sess.tyctx.tyof(HirId::new_owner(def_id)),
            )),
            BodyOwnerKind::Func | BodyOwnerKind::Lambda => self.ctx.func_ty(def_id),
        };

        match func_ty {
            InstantiatedTy::Mono(ty) => {
                let value = self.func_value(def_id, ty);
                self.function_map.insert_mono(ty, value);
            },
            InstantiatedTy::Poly(poly) => {
                poly.iter().for_each(|inst| {
                    let (ty, _) = inst.unwrap();
                    let value = self.func_value(def_id, ty);
                    self.function_map.insert_poly(ty, value);
                });
            },
        };
    }
}

impl<'ink, 'ctx> FunctionsCodeGen<'ink, 'ctx> {
    pub fn new(ctx: CodeGenCtx<'ink, 'ctx>, llvm_module: &'ctx Module<'ink>) -> Self {
        Self {
            ctx,
            llvm_module,
            function_map: Default::default(),
        }
    }

    pub fn gen_functions(mut self) -> FunctionMap<'ink> {
        self.visit_hir(self.ctx.hir);

        self.function_map
    }

    fn func_value(&mut self, def_id: DefId, ty: Ty) -> FunctionValue<'ink> {
        let ll_ty = self.ctx.conv_ty(ty).into_function_type();
        self.llvm_module.add_function(
            &self.func_name(def_id, ty),
            ll_ty,
            Some(inkwell::module::Linkage::External),
        )
    }

    fn func_name(&self, def_id: DefId, ty: Ty) -> String {
        format!(
            "{}:{}",
            self.ctx.hir.item_name(ItemId::new(def_id.into())),
            ty
        )
    }
}
