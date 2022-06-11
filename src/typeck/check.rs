use crate::{
    parser::ast::expr::ExprKind, parser::ast::expr::Lit, session::Session, typeck::ty::LitTy,
};

use super::ty::{State, TyCtx, Type};

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
            (ExprKind::Lit(_), Type::Func(_, _)) => todo!(),
            (ExprKind::Lit(_), Type::Forall(_, _)) => todo!(),
            (ExprKind::Ident(_), Type::Lit(_)) => todo!(),
            (ExprKind::Ident(_), Type::Func(_, _)) => todo!(),
            (ExprKind::Ident(_), Type::Forall(_, _)) => todo!(),
            (ExprKind::Infix(_, _, _), Type::Lit(_)) => todo!(),
            (ExprKind::Infix(_, _, _), Type::Func(_, _)) => todo!(),
            (ExprKind::Infix(_, _, _), Type::Forall(_, _)) => todo!(),
            (ExprKind::Prefix(_, _), Type::Lit(_)) => todo!(),
            (ExprKind::Prefix(_, _), Type::Func(_, _)) => todo!(),
            (ExprKind::Prefix(_, _), Type::Forall(_, _)) => todo!(),
            (ExprKind::App(_, _), Type::Lit(_)) => todo!(),
            (ExprKind::App(_, _), Type::Func(_, _)) => todo!(),
            (ExprKind::App(_, _), Type::Forall(_, _)) => todo!(),
            (ExprKind::Block(_), Type::Lit(_)) => todo!(),
            (ExprKind::Block(_), Type::Func(_, _)) => todo!(),
            (ExprKind::Block(_), Type::Forall(_, _)) => todo!(),
        }
    }
}
