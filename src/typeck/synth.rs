use crate::{
    ast::expr::Lit,
    hir::expr::{Expr, ExprKind},
};

use super::check::TypeCheck;
use super::ty::{Ctx, CtxItem, LitTy, Type, TypeKind, TypeResult};

pub trait SynthExprType {
    fn synth_type(&mut self, expr: &Expr) -> Type;
}

impl SynthExprType for Ctx {
    fn synth_type(&mut self, expr: &Expr) -> TypeResult<Type> {
        match expr.node() {
            ExprKind::Lit(lit) => Ok(Type::new(TypeKind::Lit(LitTy::from_lit_expr(*lit)))),

            ExprKind::Ident(id) => {
                if let Some(ty) = self.lookup_var(*id) {
                    return Ok(ty);
                }
                todo!("ident expr synth_type error")
            }

            ExprKind::Abs(param, body) => {
                let alpha = self.fresh_existential();
                let beta = self.fresh_existential();
                let gamma = self
                    .extended(CtxItem::ExistentialDecl(alpha, None))
                    .extended(CtxItem::ExistentialDecl(beta, None))
                    .extended(CtxItem::VarType(
                        param.clone(),
                        Type::new(TypeKind::Existential(alpha)),
                    ));

                let delta = self.check(**body, &Type::new(TypeKind::Existential(beta.clone())));

                Ok(Type::new(TypeKind::Func(
                    Type::new(TypeKind::Existential(alpha.clone())),
                    Type::new(TypeKind::Existential(beta.clone())),
                )))
            }

            ExprKind::Let(name, value, body) => {
                let (t0, gamma) = self.synth_type(expr);
            }
        }
    }
}
