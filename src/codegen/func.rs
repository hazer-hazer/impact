use std::collections::HashMap;

use inkwell::{
    types::BasicType,
    values::{BasicValueEnum, CallableValue, FunctionValue},
    AddressSpace,
};

use super::ctx::CodeGenCtx;
use crate::{
    cli::verbose,
    hir::{
        self,
        item::{ExternItem, ItemId},
        visitor::{walk_each, HirVisitor},
        BodyId, BodyOwner, BodyOwnerKind, HirId, HIR,
    },
    message::message::{impl_message_holder, MessageBuilder, MessageHolder, MessageStorage},
    mir::{InfixOp, Ty},
    resolve::def::{DefId, DefKind, DefMap},
    session::{stage_result, Stage, StageResult},
    span::sym::Ident,
    typeck::{ty::TyMap, tyctx::InstantiatedTy},
    utils::macros::match_expected,
};

// TODO: Rename `FunctionsCodeGen` to `BodyOwnersCodeGen`?

pub enum FuncInstance<'ink> {
    /// Monomorphic function instance (its type, its function value)
    Mono(Ty, FunctionValue<'ink>),
    /// polymorphic function instances, map []
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

    fn add_instance<F>(
        &mut self,
        def_id: DefId,
        func_ty: InstantiatedTy,
        mut with: F,
    ) -> Result<(), DefId>
    where
        F: FnMut(DefId, Ty) -> FunctionValue<'ink>,
    {
        match func_ty {
            InstantiatedTy::Mono(ty) => {
                let value = with(def_id, ty);
                self.insert_mono(ty, value);
                Ok(())
            },
            InstantiatedTy::Poly(poly) => {
                poly.iter().for_each(|inst| {
                    let (ty, _) = inst.unwrap();
                    let value = with(def_id, ty);
                    self.insert_poly(ty, value);
                });
                Ok(())
            },
            InstantiatedTy::None => {
                // FIXME: Is this place right for unused warning?
                Err(def_id)
            },
        }
    }

    pub fn instance(&self, def_id: DefId, ty: Ty) -> FunctionValue<'ink> {
        assert!(ty.is_instantiated());

        self.instances
            .get_flat(def_id)
            .map(|inst| match inst {
                &FuncInstance::Mono(_, mono) => mono,
                FuncInstance::Poly(poly) => poly.get_copied_unwrap(ty.id()).1,
            })
            .or_else(|| self.externals.get_flat(def_id).copied())
            .expect(&format!("Instance for function {def_id}: ({ty}) not found"))
    }

    pub fn instance_callable(&self, def_id: DefId, ty: Ty) -> CallableValue<'ink> {
        self.instance(def_id, ty)
            .as_global_value()
            .as_pointer_value()
            .try_into()
            .unwrap()
    }

    pub fn instance_basic_value(&self, def_id: DefId, ty: Ty) -> BasicValueEnum<'ink> {
        self.instance(def_id, ty)
            .as_global_value()
            .as_pointer_value()
            .into()
    }

    pub fn mono_basic_value(&self, def_id: DefId) -> BasicValueEnum<'ink> {
        self.expect_mono(def_id)
            .as_global_value()
            .as_pointer_value()
            .into()
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

    fn each_instance<F>(&self, def_id: DefId, f: F)
    where
        F: Fn(FunctionValue<'ink>),
    {
        match self.instances.get_flat(def_id).unwrap() {
            FuncInstance::Mono(_, mono) => f(*mono),
            FuncInstance::Poly(poly) => poly.iter_flat().map(|(_, val)| val).copied().for_each(f),
        }
    }
}

pub struct FunctionsCodeGen<'ink, 'ctx> {
    ctx: CodeGenCtx<'ink, 'ctx>,
    function_map: FunctionMap<'ink>,
    msg: MessageStorage,
}

impl_message_holder!(FunctionsCodeGen<'ink, 'ctx>);

impl<'ink, 'ctx> HirVisitor for FunctionsCodeGen<'ink, 'ctx> {
    fn visit_body(&mut self, &_body: &BodyId, owner: BodyOwner, _hir: &HIR) {
        if !self.ctx.should_be_built(owner.def_id) {
            return;
        }

        let def_id = owner.def_id;

        let owner_ty = self.ctx.sess.tyctx.tyof(HirId::new_owner(def_id));

        // For Func or Lambda owner we get its type (possibly polymorphic, in which case
        // we monomorphize it). For value we'll generate anonymous function of
        // type `() -> ValueType` and then immediately call it.
        let func_ty = match owner.kind {
            BodyOwnerKind::Value => {
                InstantiatedTy::Mono(Ty::func(Some(def_id), vec![Ty::unit()], owner_ty))
            },
            BodyOwnerKind::Func | BodyOwnerKind::Lambda => self.ctx.inst_ty(owner_ty, def_id),
        };

        self.function_map
            .add_instance(def_id, func_ty, |def_id, ty| {
                self.ctx.func_value(def_id, ty)
            })
            .unwrap_or_else(|def_id| self.unused_instance_warning(def_id));
    }

    fn visit_extern_item(&mut self, _: Ident, _: &ExternItem, item_id: ItemId, _: &HIR) {
        let def_id = item_id.def_id();
        let ty = self.ctx.sess.tyctx.tyof(item_id.hir_id());

        verbose!("Ty of external item: {ty}");

        if ty.is_func_like() {
            let func = self.ctx.func_value(def_id, ty);
            self.function_map.externals.insert(def_id, func);
        } else {
            todo!("Extern values")
        }
    }

    fn visit_variant(&mut self, &hir_vid: &hir::Variant, hir: &HIR) {
        let variant = hir.variant(hir_vid);

        self.visit_ident(&variant.name, hir);
        walk_each!(self, variant.fields, visit_field, hir);

        // TODO: Will be used to get ADT variant by id
        let vid = self.ctx.sess.tyctx.variant_id(variant.def_id);

        let ctor_ty = self.ctx.sess.tyctx.def_ty(variant.ctor_def_id).unwrap();
        let ctor_func_ty = self.ctx.inst_ty(ctor_ty, variant.ctor_def_id);

        // TODO: Somehow canonicalize fields order, maybe FieldId from type <=>
        //  parameter index?

        let adt_ty = self.ctx.sess.tyctx.tyof(hir_vid);

        self.function_map
            .add_instance(variant.ctor_def_id, ctor_func_ty, |_def_id, ty| {
                self.ctx.simple_func(
                    &format!("ctor_{vid}"),
                    self.ctx.conv_ty(ty).into_function_type(),
                    |_builder, params| {
                        // Fields types are constructor parameters types
                        let fields_tys = params
                            .iter()
                            .map(|param| param.get_type().as_basic_type_enum())
                            .collect::<Vec<_>>();

                        let variant_struct_ty = self.ctx.llvm_ctx.struct_type(&fields_tys, false);

                        Some(variant_struct_ty.const_named_struct(params).into())
                    },
                )
            })
            .unwrap_or_else(|def_id| self.unused_instance_warning(def_id));
    }

    fn visit_field(&mut self, field: &hir::item::Field, _hir: &HIR) {
        if let Some(accessor_def_id) = field.accessor_def_id {
            let accessor_ty = self.ctx.sess.tyctx.def_ty(accessor_def_id).unwrap();
            let accessor_func_ty = self.ctx.inst_ty(accessor_ty, accessor_def_id);
            let field_id = self.ctx.sess.tyctx.field_accessor_field_id(accessor_def_id);

            self.function_map
                .add_instance(accessor_def_id, accessor_func_ty, |_def_id, ty| {
                    // TODO: ADT variants fields
                    self.ctx.simple_func(
                        &format!("field_accessor_{field_id}"),
                        self.ctx.conv_ty(ty).into_function_type(),
                        |builder, params| {
                            let adt = params.get(0).unwrap().into_struct_value();
                            builder.build_extract_value(
                                adt,
                                field_id.inner(),
                                &format!("field_{field_id}"),
                            )
                        },
                    )
                })
                .unwrap_or_else(|def_id| self.unused_instance_warning(def_id));
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
        let op_built_item_id = ItemId::new(self.ctx.sess.def_table.builtin(op.builtin()).into());
        self.ctx.simple_func(
            op.name(),
            self.ctx
                .conv_ty(self.ctx.sess.tyctx.tyof(op_built_item_id))
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

    fn gen_main(&mut self) {
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
                builder
                    .build_call(
                        self.function_map.instance_callable(def_id, main_ty),
                        &[self.ctx.unit_value().into()],
                        &format!("main_func_call"),
                    )
                    .try_as_basic_value()
                    .unwrap_left();

                Some(self.ctx.llvm_ctx.i32_type().const_int(0, true).into())
            },
        );
    }

    fn gen_functions(&mut self) {
        self.visit_hir(self.ctx.hir);

        // Add infix operators functions
        InfixOp::each().for_each(|op| {
            let func = self.gen_infix_op(op);
            assert!(self.function_map.infix_ops.insert(op, func).is_none());
        });

        self.gen_main();
    }

    fn unused_instance_warning(&mut self, def_id: DefId) {
        let def = self.ctx.sess.def_table.def(def_id);
        MessageBuilder::warn()
            .span(def.name().span())
            .text(format!("Unused {}", def.kind()))
            .emit_single_label(self);
    }
}

impl<'ink, 'ctx> Stage<FunctionMap<'ink>, CodeGenCtx<'ink, 'ctx>> for FunctionsCodeGen<'ink, 'ctx> {
    fn run(mut self) -> StageResult<FunctionMap<'ink>, CodeGenCtx<'ink, 'ctx>> {
        self.gen_functions();
        stage_result(self.ctx, self.function_map, self.msg)
    }
}
