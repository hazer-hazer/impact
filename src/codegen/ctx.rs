use inkwell::{
    attributes::Attribute,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{
        AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType, PointerType, StructType,
    },
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};

use crate::{
    cli::verbose,
    hir::{self, item::ItemId, Map, HIR},
    message::message::impl_message_holder,
    mir::{Ty, MIR},
    resolve::{
        builtin::Builtin,
        def::{DefId, DefKind},
    },
    session::{impl_session_holder, Session, SessionHolder},
    typeck::{
        ty::{self, FloatKind, TyKind, TyMap, VariantId},
        tyctx::InstantiatedTy,
    },
};

pub struct CodeGenCtx<'ink, 'ctx> {
    pub sess: Session,
    pub hir: &'ctx HIR,
    pub mir: &'ctx MIR,

    // LLVM Context //
    pub llvm_ctx: &'ink Context,
    pub llvm_module: Module<'ink>,

    pub conv_cache: TyMap<AnyTypeEnum<'ink>>,
}

impl_session_holder!(CodeGenCtx<'ink, 'ctx>);

impl<'ink, 'ctx> CodeGenCtx<'ink, 'ctx> {
    pub fn should_be_built(&self, body_owner: DefId) -> bool {
        if matches!(
            self.sess.def_table.def(body_owner).kind(),
            DefKind::DeclareBuiltin
        ) {
            return false;
        }

        match self.sess.def_table.as_builtin(body_owner) {
            Some(bt) => match bt {
                Builtin::RefCons => false,
                _ => true,
            },
            None => true,
        }
    }

    pub fn unit_ty(&self) -> StructType<'ink> {
        self.llvm_ctx.struct_type(&[], false)
    }

    pub fn cstring_ptr_ty(&self) -> PointerType<'ink> {
        self.llvm_ctx.i8_type().ptr_type(AddressSpace::default())
    }

    /// Get uint type to fit sum number, e.g. count of variants in ADT
    pub fn fitted_uint(&self, num: usize) -> IntType<'ink> {
        let ranges: &[u32] = &[8, 16, 32, 64];

        ranges
            .iter()
            .copied()
            .find_map(|range| {
                if (0..(range as usize * 8)).contains(&num) {
                    Some(self.llvm_ctx.custom_width_int_type(range).into())
                } else {
                    None
                }
            })
            .expect(&format!(
                "Failed to fit number {num} in range of available int types"
            ))
    }

    // TODO: Cache
    pub fn conv_ty(&self, ty: Ty) -> AnyTypeEnum<'ink> {
        if let Some(&conv) = self.conv_cache.get_flat(ty.id()) {
            return conv;
        }

        match ty.kind() {
            TyKind::Unit => self.unit_ty().into(),
            TyKind::Bool => self.llvm_ctx.bool_type().into(),
            TyKind::Int(kind) => self.llvm_ctx.custom_width_int_type(kind.bits()).into(),
            TyKind::Float(kind) => match kind {
                FloatKind::F32 => self.llvm_ctx.f32_type().into(),
                FloatKind::F64 => self.llvm_ctx.f64_type().into(),
            },
            &TyKind::Ref(inner) => self
                .conv_basic_ty(inner)
                .ptr_type(AddressSpace::default())
                .into(),
            TyKind::Str => self.cstring_ptr_ty().into(),
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => self
                .conv_basic_ty(*body)
                .fn_type(
                    &params
                        .iter()
                        .map(|&param| self.conv_basic_ty(param).into())
                        .collect::<Vec<_>>(),
                    false,
                )
                .into(),
            TyKind::Existential(_) | TyKind::Forall(..) | TyKind::Error | TyKind::Var(_) => {
                unreachable!("Type {} must not exist on codegen stage", ty)
            },
            TyKind::Kind(_) => todo!(),
            TyKind::Adt(_) => self.conv_adt_ty(ty),
            TyKind::Struct(data) => self.conv_struct_ty(data),
            // TyKind::Struct(data) => conv,
        }
    }

    pub fn conv_adt_ty(&self, adt_ty: Ty) -> AnyTypeEnum<'ink> {
        if let Some(&conv) = self.conv_cache.get_flat(adt_ty.id()) {
            return conv;
        }

        let adt_ty = adt_ty.as_adt().unwrap();

        assert!(!adt_ty.variants.is_empty());

        let max_variant_size = adt_ty
            .max_variant_size()
            .expect("[BUG] Unknown size of ADT type must be checked before codegen");

        let fitted_tag_ty = self.fitted_uint(adt_ty.variants.len());

        self.llvm_ctx
            .struct_type(
                &[
                    fitted_tag_ty.into(),
                    self.llvm_ctx.i8_type().array_type(max_variant_size).into(),
                ],
                false,
            )
            .into()
    }

    pub fn adt_ty_fitted_tag_ty(&self, adt_ty: Ty) -> IntType<'ink> {
        self.fitted_uint(adt_ty.as_adt().unwrap().variants.len())
    }

    pub fn conv_variant_fields_tys(
        &self,
        adt_ty: &ty::Adt,
        vid: VariantId,
    ) -> Vec<BasicTypeEnum<'ink>> {
        adt_ty
            .variants
            .get(vid)
            .unwrap()
            .fields
            .iter()
            .map(|field| self.conv_basic_ty(field.ty))
            .collect()
    }

    pub fn conv_struct_ty(&self, struct_ty: &ty::Struct) -> AnyTypeEnum<'ink> {
        self.llvm_ctx
            .struct_type(
                &struct_ty
                    .fields
                    .iter()
                    .copied()
                    .map(|f| self.conv_basic_ty(f.ty))
                    .collect::<Vec<_>>(),
                false,
            )
            .into()
    }

    pub fn conv_basic_ty(&self, ty: Ty) -> BasicTypeEnum<'ink> {
        if let Some(&conv) = self.conv_cache.get_flat(ty.id()) {
            return conv.try_into().unwrap();
        }

        match self.conv_ty(ty) {
            AnyTypeEnum::FunctionType(func_ty) => func_ty.ptr_type(AddressSpace::default()).into(),
            conv @ _ => conv,
        }
        .try_into()
        .expect(&format!("Failed to convert {} to BasicType", ty))
    }

    pub fn inst_ty(&self, ty: Ty, def_id: DefId) -> InstantiatedTy<Ty> {
        let inst_ty = self.sess.tyctx.instantiated_ty(ty, def_id);

        match inst_ty {
            InstantiatedTy::Mono(mono) => InstantiatedTy::Mono(mono),
            InstantiatedTy::Poly(poly) => InstantiatedTy::Poly(
                poly.iter()
                    .map(|res| res.map(|(ty, expr)| (ty, expr)))
                    .collect(),
            ),
            InstantiatedTy::None => InstantiatedTy::None,
        }
    }

    pub fn variant_id_by_tag(&self, value: IntValue<'ink>) -> VariantId {
        (value
            .get_zero_extended_constant()
            .expect("[BUG] ADT with > 8 bytes variants are not supported") as usize)
            .into()
    }

    pub fn tag_by_variant_id(&self, adt_ty: Ty, vid: VariantId) -> IntValue<'ink> {
        self.adt_ty_fitted_tag_ty(adt_ty)
            .const_int(vid.inner().into(), false)
    }

    // Functions //
    pub fn simple_func(
        &self,
        name: &str,
        ty: FunctionType<'ink>,
        body: impl Fn(&Builder<'ink>, &[BasicValueEnum<'ink>]) -> Option<BasicValueEnum<'ink>>,
    ) -> FunctionValue<'ink> {
        // assert!(ty.is_func_like());
        // let ll_ty = self.conv_ty(ty).into_function_type();
        let func = self
            .llvm_module
            .add_function(name, ty, Some(Linkage::External));

        let builder = self.llvm_ctx.create_builder();
        let entry_bb = self.llvm_ctx.append_basic_block(func, "entry");
        builder.position_at_end(entry_bb);

        let result = body(&builder, &func.get_params());

        if let Some(result) = result {
            builder.build_return(Some(&result));
        } else {
            builder.build_return(None);
        }

        func
    }

    pub fn func_value(&mut self, def_id: DefId, ty: Ty) -> FunctionValue<'ink> {
        let ll_ty = self.conv_ty(ty).into_function_type();
        self.llvm_module.add_function(
            &self.func_name(def_id, ty),
            ll_ty,
            Some(inkwell::module::Linkage::External),
        )
    }

    fn func_name(&self, def_id: DefId, _ty: Ty) -> String {
        if def_id == self.sess.def_table.expect_main_func() {
            return "_main".to_string();
        }
        format!(
            "{}",
            self.hir
                .item(ItemId::new(def_id.into()))
                .name()
                .original_string(),
            // ty.id()
        )
    }

    // pub fn simple_func(
    //     &self,
    //     ty: Ty,
    //     deep_func_modifier: impl Fn(FunctionValue<'ink>),
    //     body: impl Fn(&Builder<'ink>, Vec<BasicValueEnum<'ink>>) ->
    // BasicValueEnum<'ink>, ) -> FunctionValue<'ink> {
    //     assert!(ty.is_func_like());
    //     self._simple_func(Vec::new(), ty, deep_func_modifier, body)
    //         .0
    // }

    // fn _simple_func(
    //     &self,
    //     mut params: Vec<BasicValueEnum<'ink>>,
    //     ty: Ty,
    //     deep_func_modifier: impl Fn(FunctionValue<'ink>),
    //     body: impl Fn(&Builder<'ink>, Vec<BasicValueEnum<'ink>>) ->
    // BasicValueEnum<'ink>, ) -> (FunctionValue<'ink>,
    // Option<BasicValueEnum<'ink>>) {     let ll_ty =
    // self.conv_ty(ty).into_function_type();     let func = self
    //         .llvm_module
    //         .add_function("anon", ll_ty, Some(Linkage::Internal));

    //     params.push(func.get_nth_param(0).unwrap());

    //     deep_func_modifier(func);

    //     let builder = self.llvm_ctx.create_builder();
    //     let entry_bb = self.llvm_ctx.append_basic_block(func, "entry");
    //     builder.position_at_end(entry_bb);

    //     let (func, result) = if ty.return_ty().is_func_like() {
    //         (
    //             self._simple_func(params, ty.return_ty(), deep_func_modifier,
    // body)                 .0,
    //             None,
    //         )
    //     } else {
    //         (func, Some(body(&builder, params)))
    //     };

    //     if let Some(result) = result {
    //         builder.build_return(Some(&result));
    //     } else {
    //         builder.build_return(Some(
    //             &func
    //                 .as_global_value()
    //                 .as_pointer_value()
    //                 .as_basic_value_enum(),
    //         ));
    //     }

    //     (func, None)
    // }

    // Function attributes //
    pub fn always_inline(&self, func: FunctionValue<'ink>) {
        let always_inline = Attribute::get_named_enum_kind_id("alwaysinline");
        assert_ne!(always_inline, 0);
        let attr = self.llvm_ctx.create_enum_attribute(always_inline, 1);
        func.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
    }

    // Values //
    pub fn unit_value(&self) -> BasicValueEnum<'ink> {
        self.unit_ty().const_zero().into()
    }

    pub fn cstring_ptr_value(&self, bytes: &[u8]) -> PointerValue<'ink> {
        let str = self.llvm_ctx.const_string(bytes, true);
        verbose!("str value: {str}");
        let global = self
            .llvm_module
            .add_global(str.get_type(), None, "string_slice");
        global.set_constant(true);
        global.set_initializer(&str);
        // global.set_visibility(GlobalVisibility::Default);
        // global.set_alignment(1);
        global.as_pointer_value()
    }
}
