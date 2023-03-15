use inkwell::{
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicTypeEnum, FunctionType},
};

use crate::{
    hir::{item::ItemId, HirId, HIR},
    mir::{Ty, MIR},
    resolve::def::DefId,
    session::Session,
    span::span::Symbol,
    typeck::ty::{FloatKind, PrimTy, TyKind},
};

pub struct CodeGenCtx<'ink, 'ctx> {
    pub sess: &'ctx Session,
    pub mir: &'ctx MIR,
    pub hir: &'ctx HIR,

    // LLVM Context //
    pub llvm_ctx: &'ink Context,
    pub llvm_module: &'ink Module<'ink>,
}

impl<'ink, 'ctx> CodeGenCtx<'ink, 'ctx> {
    pub fn conv_ty(&self, ty: Ty) -> AnyTypeEnum<'ink> {
        match ty.kind() {
            TyKind::Unit => self.llvm_ctx.i8_type().into(),
            TyKind::Prim(prim) => match prim {
                PrimTy::Bool => self.llvm_ctx.bool_type().into(),
                PrimTy::Int(kind) => self
                    .llvm_ctx
                    .custom_width_int_type(kind.bits().into())
                    .into(),
                PrimTy::Float(kind) => match kind {
                    FloatKind::F32 => self.llvm_ctx.f32_type().into(),
                    FloatKind::F64 => self.llvm_ctx.f64_type().into(),
                },
                PrimTy::String => todo!(),
            },
            TyKind::Func(param, body) => todo!(),
            TyKind::Func(_, _)
            | TyKind::Existential(_)
            | TyKind::Forall(_, _)
            | TyKind::Error
            | TyKind::Var(_) => {
                unreachable!()
            },
        }
    }

    pub fn conv_basic_ty(&self, ty: Ty) -> BasicTypeEnum<'ink> {
        self.conv_ty(ty).try_into().unwrap()
    }

    pub fn func_ty(&self, def_id: DefId) -> FunctionType<'ink> {
        let ty = self
            .sess
            .tyctx
            .instantiated_func_ty(HirId::new_owner(def_id))
            .unwrap();

        self.conv_ty(ty).into_function_type()
    }
}
