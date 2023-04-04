use inkwell::{
    attributes::Attribute,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, PointerType, StructType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};

use crate::{
    hir::{BodyOwner, OwnerId, HIR},
    mir::{Ty, MIR},
    resolve::{builtin::Builtin, def::DefId},
    session::Session,
    typeck::{
        ty::{FloatKind, TyKind},
        tyctx::InstantiatedTy,
    },
};

#[derive(Clone, Copy)]
pub struct CodeGenCtx<'ink, 'ctx> {
    pub sess: &'ctx Session,
    pub mir: &'ctx MIR,
    pub hir: &'ctx HIR,

    // LLVM Context //
    pub llvm_ctx: &'ink Context,
    pub llvm_module: &'ctx Module<'ink>,
}

impl<'ink, 'ctx> CodeGenCtx<'ink, 'ctx> {
    pub fn should_be_built(&self, body_owner: DefId) -> bool {
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

    pub fn conv_ty(&self, ty: Ty) -> AnyTypeEnum<'ink> {
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
            TyKind::Existential(_) | TyKind::Forall(_, _) | TyKind::Error | TyKind::Var(_) => {
                unreachable!("Type {} must not exist on codegen stage", ty)
            },
            TyKind::Kind(_) => todo!(),
        }
    }

    pub fn conv_basic_ty(&self, ty: Ty) -> BasicTypeEnum<'ink> {
        match self.conv_ty(ty) {
            AnyTypeEnum::FunctionType(func_ty) => func_ty.ptr_type(AddressSpace::default()).into(),
            conv @ _ => conv,
        }
        .try_into()
        .expect(&format!("Failed to convert {} to BasicType", ty))
    }

    pub fn func_ty(&self, def_id: DefId) -> InstantiatedTy<Ty> {
        let inst_ty = self.sess.tyctx.instantiated_ty(def_id);

        match inst_ty {
            InstantiatedTy::Mono(mono) => InstantiatedTy::Mono(mono),
            InstantiatedTy::Poly(poly) => InstantiatedTy::Poly(
                poly.iter()
                    .map(|res| res.map(|(ty, expr)| (ty, expr)))
                    .collect(),
            ),
        }

        // .map(|res| res.map(|ty| self.conv_ty(ty).into_function_type()))
        // .collect()
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
            .add_function(name, ty, Some(Linkage::Internal));

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

    // pub fn simple_func(
    //     &self,
    //     ty: Ty,
    //     deep_func_modifier: impl Fn(FunctionValue<'ink>),
    //     body: impl Fn(&Builder<'ink>, Vec<BasicValueEnum<'ink>>) -> BasicValueEnum<'ink>,
    // ) -> FunctionValue<'ink> {
    //     assert!(ty.is_func_like());
    //     self._simple_func(Vec::new(), ty, deep_func_modifier, body)
    //         .0
    // }

    // fn _simple_func(
    //     &self,
    //     mut params: Vec<BasicValueEnum<'ink>>,
    //     ty: Ty,
    //     deep_func_modifier: impl Fn(FunctionValue<'ink>),
    //     body: impl Fn(&Builder<'ink>, Vec<BasicValueEnum<'ink>>) -> BasicValueEnum<'ink>,
    // ) -> (FunctionValue<'ink>, Option<BasicValueEnum<'ink>>) {
    //     let ll_ty = self.conv_ty(ty).into_function_type();
    //     let func = self
    //         .llvm_module
    //         .add_function("anon", ll_ty, Some(Linkage::Internal));

    //     params.push(func.get_nth_param(0).unwrap());

    //     deep_func_modifier(func);

    //     let builder = self.llvm_ctx.create_builder();
    //     let entry_bb = self.llvm_ctx.append_basic_block(func, "entry");
    //     builder.position_at_end(entry_bb);

    //     let (func, result) = if ty.return_ty().is_func_like() {
    //         (
    //             self._simple_func(params, ty.return_ty(), deep_func_modifier, body)
    //                 .0,
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
        let global = self
            .llvm_module
            .add_global(str.get_type(), None, "string_slice");
        global.set_constant(true);
        global.set_initializer(&str);
        global.as_pointer_value()
    }
}
