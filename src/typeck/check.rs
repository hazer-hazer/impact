use crate::{
    ast::expr::Lit,
    hir::expr::{Expr, ExprKind},
    session::Session,
    typeck::ty::LitTy,
};

use super::ty::{CtxEl, State, TyCtx, Type};

pub trait NodeTyCheck {
    fn check(&self, sess: &mut Session, state: &mut State, ctx: &TyCtx, ty: &Type) -> TyCtx;
}

impl NodeTyCheck for Expr {
    fn check(&self, sess: &mut Session, state: &mut State, ctx: &TyCtx, ty: &Type) -> TyCtx {
        match (self.node(), ty) {
            (ExprKind::Lit(lit), Type::Lit(ty)) => {
                assert!(match (lit, ty) {
                    (Lit::Bool(_), LitTy::Bool) => true,
                    (Lit::Int(_), LitTy::Int) => true,
                    (Lit::String(_), LitTy::String) => true,
                    _ => false,
                });
                ctx.clone()
            }
            (ExprKind::Abs(param, body), Type::Func(param_ty, ret_ty)) => {
                let typed_var = CtxEl::TypedVar(param.name(), *param_ty.clone());
                let ctx = ctx.extended(typed_var.clone());
                body.check(sess, state, &ctx, ret_ty).drop(typed_var)
            }
            (_, Type::Forall(ty_name, ty)) => {
                let var = CtxEl::Var(*ty_name);
                let ctx = ctx.extended(var.clone());
                self.check(sess, state, &ctx, ty).drop(var)
            }
            (_, _) => todo!(),
        }
    }
}
