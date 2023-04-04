use std::collections::HashMap;

use inkwell::{
    types::BasicType,
    values::{BasicValueEnum, CallableValue, FunctionValue},
    AddressSpace,
};

use crate::{
    cli::verbose,
    hir::{
        item::{ExternItem, ItemId},
        visitor::HirVisitor,
        BodyId, BodyOwner, BodyOwnerKind, HirId, HIR,
    },
    message::message::{MessageStorage, MessagesResult, MsgResult},
    mir::{InfixOp, Ty},
    resolve::def::{DefId, DefKind, DefMap},
    span::span::Ident,
    typeck::{
        ty::{IntKind, TyMap},
        tyctx::InstantiatedTy,
    },
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
pub struct FunctionMap<'ink> {
    instances: DefMap<FuncInstance<'ink>>,
    infix_ops: HashMap<InfixOp, FunctionValue<'ink>>,
    externals: DefMap<FunctionValue<'ink>>,
}

impl<'ink> FunctionMap<'ink> {
    pub fn expect_mono(&self, def_id: DefId) -> FunctionValue<'ink> {
        match self.instances.get_unwrap(def_id) {
            &FuncInstance::Mono(_, mono) => Some(mono),
            FuncInstance::Poly(_) => panic!(),
        }
        .or_else(|| self.externals.get_flat(def_id).copied())
        .unwrap()
    }

    pub fn infix(&self, op: InfixOp) -> FunctionValue<'ink> {
        self.infix_ops.get(&op).copied().unwrap()
    }

    pub fn instance(&self, def_id: DefId, ty: Ty) -> FunctionValue<'ink> {
        assert!(ty.is_instantiated());
        if let Some(inst) = self.instances.get_flat(def_id) {
            match inst {
                &FuncInstance::Mono(_, mono) => mono,
                FuncInstance::Poly(poly) => poly.get_copied_unwrap(ty.id()).1,
            }
        } else {
            self.externals.get_copied_unwrap(def_id)
        }
    }

    fn insert_mono(&mut self, ty: Ty, value: FunctionValue<'ink>) {
        assert!(
            ty.is_instantiated() && ty.func_def_id().is_some(),
            "Cannot insert mono function type {ty} without DefId"
        );
        assert!(self
            .instances
            .insert(ty.func_def_id().unwrap(), FuncInstance::Mono(ty, value))
            .is_none());
    }

    fn insert_poly(&mut self, ty: Ty, value: FunctionValue<'ink>) {
        assert!(ty.is_instantiated());
        self.instances
            .upsert(ty.func_def_id().unwrap(), || {
                FuncInstance::Poly(Default::default())
            })
            .add_poly(ty, value);
    }

    pub fn iter_internal(&self) -> impl Iterator<Item = (DefId, &FuncInstance<'ink>)> + '_ {
        self.instances.iter_enumerated_flat()
    }
}

pub struct FunctionsCodeGen<'ink, 'ctx> {
    ctx: CodeGenCtx<'ink, 'ctx>,
    function_map: FunctionMap<'ink>,
    msg: MessageStorage,
}

impl<'ink, 'ctx> HirVisitor for FunctionsCodeGen<'ink, 'ctx> {
    fn visit_body(&mut self, &_body: &BodyId, owner: BodyOwner, _hir: &HIR) {
        if !self.ctx.should_be_built(owner.def_id) {
            return;
        }

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
                Some(def_id),
                vec![Ty::unit()],
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

    fn visit_extern_item(&mut self, _: Ident, _: &ExternItem, item_id: ItemId, _: &HIR) {
        let def_id = item_id.def_id();
        let ty = self.ctx.sess.tyctx.tyof(def_id.into());

        verbose!("Ty of external item: {ty}");

        if ty.is_func_like() {
            let func = self.func_value(def_id, ty);
            self.function_map.externals.insert(def_id, func);
        } else {
            todo!("Extern values")
        }
    }
}

impl<'ink, 'ctx> FunctionsCodeGen<'ink, 'ctx> {
    pub fn new(ctx: CodeGenCtx<'ink, 'ctx>) -> Self {
        Self {
            ctx,
            function_map: Default::default(),
            msg: Default::default(),
        }
    }

    fn gen_infix_op(&mut self, op: InfixOp) -> FunctionValue<'ink> {
        self.ctx.simple_func(
            op.name(),
            self.ctx
                .conv_ty(
                    self.ctx
                        .sess
                        .tyctx
                        .tyof(self.ctx.sess.def_table.builtin(op.builtin()).into()),
                )
                .into_function_type(),
            |builder, params| {
                // verbose!(
                //     "{}",
                //     params
                //         .iter()
                //         .map(ToString::to_string)
                //         .collect::<Vec<_>>()
                //         .join(", ")
                // );
                assert_eq!(
                    params.len(),
                    2,
                    "Infix operator function {} expected {} parameters ",
                    op.name(),
                    2
                );
                let lhs = params[0];
                let rhs = params[1];
                Some(
                    match op {
                        InfixOp::AddInt => builder.build_int_add(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            op.name(),
                        ),
                        InfixOp::SubInt => builder.build_int_sub(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            op.name(),
                        ),
                    }
                    .into(),
                )
            },
        )
    }

    pub fn gen_main(&mut self) {
        let def_id = self.ctx.sess.def_table.expect_main_func();

        let real_main_ty = self.ctx.llvm_ctx.i32_type().fn_type(
            &[
                self.ctx.llvm_ctx.i32_type().into(),
                self.ctx
                    .llvm_ctx
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .ptr_type(AddressSpace::default())
                    .into(),
            ],
            false,
        );

        self.ctx.simple_func(
            "main",
            real_main_ty,
            // Ty::func(Some(def_id), vec![], Ty::int(IntKind::I32)),
            |builder, _params| {
                let main_ty = self.ctx.sess.tyctx.tyof(HirId::new_owner(def_id));
                let main_func: CallableValue<'ink> = self
                    .function_map
                    .instance(def_id, main_ty)
                    .as_global_value()
                    .as_pointer_value()
                    .try_into()
                    .unwrap();
                builder
                    .build_call(
                        main_func,
                        &[self.ctx.unit_value().into()],
                        &format!("main_func_call"),
                    )
                    .try_as_basic_value()
                    .unwrap_left();

                Some(self.ctx.llvm_ctx.i32_type().const_int(0, true).into())
            },
        );
    }

    pub fn gen_functions(mut self) -> MessagesResult<FunctionMap<'ink>> {
        self.visit_hir(self.ctx.hir);

        // Add infix operators functions
        InfixOp::each().for_each(|op| {
            let func = self.gen_infix_op(op);
            assert!(self.function_map.infix_ops.insert(op, func).is_none());
        });

        self.gen_main();

        self.msg.error_checked_result(self.function_map)
    }

    fn func_value(&mut self, def_id: DefId, ty: Ty) -> FunctionValue<'ink> {
        let ll_ty = self.ctx.conv_ty(ty).into_function_type();
        self.ctx.llvm_module.add_function(
            &self.func_name(def_id, ty),
            ll_ty,
            Some(inkwell::module::Linkage::External),
        )
    }

    fn func_name(&self, def_id: DefId, _ty: Ty) -> String {
        format!(
            "{}",
            self.ctx
                .hir
                .item_name(ItemId::new(def_id.into()))
                .original_string(),
            // ty.id()
        )
    }
}
