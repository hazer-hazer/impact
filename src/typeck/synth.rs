use crate::{
    ast::expr::Lit,
    hir::expr::{Expr, ExprKind},
    session::Session,
};

use super::{
    check::NodeTyCheck,
    ty::{CtxEl, LitTy, State, TyCtx, Type},
};

pub trait NodeTySynth {
    fn synth(&self, sess: &mut Session, state: &mut State, ctx: &TyCtx) -> (Type, TyCtx);
}

impl NodeTySynth for Expr {
    fn synth(&self, sess: &mut Session, state: &mut State, ctx: &TyCtx) -> (Type, TyCtx) {
        match self.node() {
            ExprKind::Lit(lit) => (
                match lit {
                    Lit::Bool(_) => Type::Lit(LitTy::Bool),
                    Lit::Int(_) => Type::Lit(LitTy::Int),
                    Lit::String(_) => Type::Lit(LitTy::String),
                },
                ctx.clone(),
            ),
            ExprKind::Ident(ident) => {
                // TODO: Anno
                todo!()
            }
            ExprKind::Infix(_, _, _) => todo!(),
            ExprKind::Prefix(_, _) => todo!(),
            ExprKind::Abs(param, body) => {
                let alpha = state.fresh(sess);
                let beta = state.fresh(sess);
                let gamma = ctx
                    .extended(CtxEl::Exist(alpha.clone()))
                    .extended(CtxEl::Exist(beta.clone()))
                    .extended(CtxEl::TypedVar(
                        param.name(),
                        Type::Existential(alpha.clone()),
                    ));

                let delta = body
                    .check(sess, state, &gamma, &Type::Existential(beta.clone()))
                    .drop(CtxEl::TypedVar(
                        param.name(),
                        Type::Existential(alpha.clone()),
                    ));

                return (
                    Type::Func(
                        Box::new(Type::Existential(alpha.clone())),
                        Box::new(Type::Existential(beta.clone())),
                    ),
                    delta,
                );
            }
            ExprKind::App(lhs, arg) => {
                let (a, theta) = lhs.synth(sess, state, ctx);
                todo!()
            },
            ExprKind::Block(_) => todo!(),
            ExprKind::Let(name, value, body) => {
                let (t0, gamma) = value.synth(sess, state, ctx);
                let theta = gamma.extended(CtxEl::TypedVar(name.name(), t0));

                let (t1, delta) = body.synth(sess, state, &theta);

                todo!()
            },
        }
    }
}
