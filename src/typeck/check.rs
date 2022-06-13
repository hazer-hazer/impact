use crate::{hir::expr::{Expr, ExprKind}, typeck::ty::{TypeKind, LitTy, TypeError}, ast::expr::Lit};

use super::ty::{Type, TypeResult, Ctx, CtxItem};

pub trait TypeCheck {
    fn check(&mut self, expr: Expr, ty: &Type) -> TypeResult<()>;
}

impl TypeCheck for Ctx {
    fn check(&mut self, expr: Expr, ty: &Type) -> TypeResult<()> {
        self.ty_wf(ty)?;

        match (expr.node(), ty.kind()) {
            (ExprKind::Lit(lit), TypeKind::Lit(lit_ty)) => {
                let expr_ty = LitTy::from_lit_expr(*lit);
                if expr_ty == *lit_ty {
                    Ok(())
                } else {
                    Err(TypeError::new(format!("Types {} and {} do not match", expr_ty, lit_ty)))
                }
            }

            (ExprKind::Abs(param, body), TypeKind::Func(param_ty, body_ty)) => {
                let typed_var = CtxItem::VarType(*param, param_ty.clone());
                let mut gamma = self.extended(typed_var);
                gamma.check(expr, body_ty)
            }

            (_, TypeKind::Forall(alpha, a)) => {
                let var = CtxItem::TypeDecl(alpha.clone());
                let mut gamma = self.extended(var.clone());
                gamma.check(expr, a)
            }

            (_, _) => {
                todo!()
            }
        }
    }
}
