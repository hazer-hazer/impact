use inkwell::{
    context::Context,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
};

use crate::{
    hir::{expr::Expr, HirId, HIR},
    mir::{Ty, MIR},
    resolve::def::DefId,
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
}

impl<'ink, 'ctx> CodeGenCtx<'ink, 'ctx> {
    pub fn conv_ty(&self, ty: Ty) -> AnyTypeEnum<'ink> {
        match ty.kind() {
            TyKind::Unit => self.llvm_ctx.i8_type().into(),
            TyKind::Bool => self.llvm_ctx.bool_type().into(),
            TyKind::Int(kind) => self
                .llvm_ctx
                .custom_width_int_type(kind.bits().into())
                .into(),
            TyKind::Float(kind) => match kind {
                FloatKind::F32 => self.llvm_ctx.f32_type().into(),
                FloatKind::F64 => self.llvm_ctx.f64_type().into(),
            },
            TyKind::String => todo!(),
            &TyKind::Func(param, body) | &TyKind::FuncDef(_, param, body) => self
                .conv_basic_ty(body)
                .fn_type(&[self.conv_basic_ty(param).into()], false)
                .into(),
            TyKind::Existential(_) | TyKind::Forall(_, _) | TyKind::Error | TyKind::Var(_) => {
                unreachable!()
            },
        }
    }

    pub fn conv_basic_ty(&self, ty: Ty) -> BasicTypeEnum<'ink> {
        self.conv_ty(ty)
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
}
