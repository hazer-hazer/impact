use crate::{
    parser::ast::expr::ExprKind, parser::ast::expr::Lit, session::Session, typeck::ty::LitTy,
};

use super::ty::{State, TyCtx, Type, CtxEl};

trait NodeTyCheck {
    fn check(&self, sess: &mut Session, state: &mut State, ctx: &TyCtx, ty: &Type) -> TyCtx;
}

impl NodeTyCheck for ExprKind {
    fn check(&self, sess: &mut Session, state: &mut State, ctx: &TyCtx, ty: &Type) -> TyCtx {
        match (self, ty) {
            (ExprKind::Lit(lit), Type::Lit(ty)) => {
                assert!(match (lit, ty) {
                    (Lit::Bool(_), LitTy::Bool) => true,
                    (Lit::Int(_), LitTy::Int) => true,
                    (Lit::String(_), LitTy::String) => true,
                    _ => false,
                });
                ctx.clone()
            }
            _ => todo!()
        }
    }
}
