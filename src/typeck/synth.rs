use crate::{
    hir::expr::{Expr, ExprKind},
};

use super::check::TypeCheck;
use super::ty::{Ctx, CtxItem, LitTy, Type, TypeKind, TypeResult};

pub trait SynthExprType {
    fn synth_type(&mut self, expr: &Expr) -> TypeResult<(Type, Ctx)>;
    fn synth_app_type(&mut self, expr: &Expr, a_ty: &Type) -> TypeResult<(Type, Ctx)>;
}

impl SynthExprType for Ctx {
    fn synth_type(&mut self, expr: &Expr) -> TypeResult<(Type, Ctx)> {
        match expr.node() {
            /*
             * Unit/literal type synthesis.
             * ———————————————— 1I⇒
             * Γ ⊢ () ⇒ 1 ⊣ Γ
             *
             * We always know type for a unit type (for literals too).
             */
            &ExprKind::Lit(lit) => Ok((
                Type::new(TypeKind::Lit(LitTy::from_lit_expr(lit))),
                self.clone(),
            )),

            /*
             * Variable type synthesis.
             *  (x : A) ∈ Γ
             * —————————————— Var
             * Γ ⊢ x ⇒ Α ⊣ Γ
             *
             * To synthesize type of a variable we get type annotated on it.
             */
            &ExprKind::Ident(id) => {
                // (x : A) ∈ Γ
                if let Some(ty) = self.lookup_var(id) {
                    // Γ ⊢ x ⇒ Α ⊣ Γ
                    Ok((ty, self.clone()))
                } else {
                    todo!("ident expr synth_type error")
                }
            }

            /*
             * Abstraction type synthesis.
             * Γ, α^, β^, x : α^ ⊢ e ⇐ β^ ⊣ Δ, x : α^, Θ
             * —————————————————————————————————————————— →I⇒
             *       Γ ⊢ λx. e ⇒ α^ → β^ ⊣ Δ
             */
            &ExprKind::Abs(param, body) => {
                // α^
                let alpha = self.fresh_existential();

                // β^
                let beta = self.fresh_existential();

                let mut gamma = self
                    // Γ, α^
                    .extended(CtxItem::ExistentialDecl(alpha, None))
                    // Γ, β^
                    .extended(CtxItem::ExistentialDecl(beta, None))
                    // Γ, x : α^
                    .extended(CtxItem::TypedVar(
                        param.clone(),
                        Type::new(TypeKind::Existential(alpha)),
                    ));

                // e ⇐ β^ ⊣ Δ
                let delta = gamma.check(&*body, &Type::new(TypeKind::Existential(beta.clone())))?;

                // Synthesize α^ → β^ checked in the Δ context
                Ok((
                    Type::new(TypeKind::Func(
                        Type::new(TypeKind::Existential(alpha.clone())),
                        Type::new(TypeKind::Existential(beta.clone())),
                    )),
                    delta,
                ))
            }

            &ExprKind::Let(name, value, body) => {
                let (t0, gamma) = self.synth_type(&value)?;
                let mut theta = gamma.extended(CtxItem::TypedVar(name, t0.clone()));

                let (t1, delta) = theta.synth_type(&body)?;

                Ok((t1, delta.add_in_place(CtxItem::TypedVar(name, t0), vec![])))
            }

            /*
             * Gold quote fund: An algorithmic context can be viewed as a substitution for
             *  its solved existential variables.
             *
             * Note: "Γ ⊢ A • e ⇒⇒ C ⊣ Δ" stands for "Under input context Γ,
             *  applying a function of type A to e synthesizes type C, with output context Δ"
             *
             * The entry point of application type synthesis.
             * We synthesize type of e₁ and, if e₁ is a function type Α (otherwise we have a type error),
             *  we apply Α to e₂ to synthesize type C, our result type (or return type) of the application.
             * The context Θ (theta) is used as temporal context.
             *
             * Γ ⊢ e₁ ⇒ Α ⊣ Θ  Θ ⊢ [Θ]Α • e₂ ⇒⇒ C ⊣ Δ
             * ————————————————————————————————————————— →E
             *           Γ ⊢ e₁ e₂ ⇒ C ⊣ Δ
             *
             *
             * Monotype application type synthesis.
             * If we can check e against type Α, applying Α → C to e synthesizes type C.
             *
             *      Γ ⊢ e ⇐ Α ⊣ Δ
             * ———————————————————————— →App
             * Γ ⊢ Α → C • e ⇒⇒ C ⊣ Δ
             *
             *
             * Application, containing universally quantified type variables, type synthesis.
             * Assuming we have an existential α^ that, by substituting to α in function Α, and,
             *  applying that function to e synthesizes type C, we can synthesize type C for all α (∀α),
             *  applying ∀α. Α to e.
             * What's wrong with my writing style... this paper is killing me
             *
             * Γ, α^ ⊢ [α^/α]Α • e ⇒⇒ C ⊣ Δ
             * —————————————————————————————— ∀App
             *   Γ ⊢ ∀α. Α • e ⇒⇒ C ⊣ Δ
             *
             *
             * Application with existentials.
             * If, by substituting context Γ to types α^₁, α^₂ and α^ = α^₁ → α^₂, e checks against type α^₁,
             *  and applying α^ to e we can synthesize type α^₂.
             *
             * Γ[α^₂, α^₁, α^ = α^₁ → α^₂] ⊢ e ⇐ α^₁ ⊣ Δ
             * —————————————————————————————————————————— α^App
             *       Γ[α^] ⊢ α^ • e ⇒⇒ α^₂ ⊣ Δ
             */
            &ExprKind::App(lhs, arg) => {
                let (a, mut theta) = self.synth_type(&lhs)?;

                theta.synth_app_type(&arg, &a.apply_ctx(&theta))
            }
        }
    }

    fn synth_app_type(&mut self, expr: &Expr, a_ty: &Type) -> TypeResult<(Type, Ctx)> {
        match &a_ty.kind() {
            &TypeKind::Func(a, c) => {
                let delta = self.check(expr, a)?;
                Ok((c.clone(), delta))
            }

            &TypeKind::Forall(alpha, a) => {
                let alpha_ex = self.fresh_existential();
                let mut gamma = self.extended(CtxItem::ExistentialDecl(alpha_ex, None));
                let substituted_a = self.substitute(a);

                gamma.synth_app_type(expr, &substituted_a)
            }

            &TypeKind::Existential(id) => {
                let alpha_ex_1 = self.fresh_existential();
                let alpha_ex_2 = self.fresh_existential();
                let mut gamma = self.add_in_place(
                    CtxItem::ExistentialDecl(*id, None),
                    vec![
                        CtxItem::ExistentialDecl(alpha_ex_2, None),
                        CtxItem::ExistentialDecl(alpha_ex_1, None),
                        CtxItem::ExistentialDecl(
                            *id,
                            Some(Type::new(TypeKind::Func(
                                Type::new(TypeKind::Existential(alpha_ex_1)),
                                Type::new(TypeKind::Existential(alpha_ex_2)),
                            ))),
                        ),
                    ],
                );

                let delta = gamma.check(expr, &Type::new(TypeKind::Existential(alpha_ex_1)))?;

                Ok((Type::new(TypeKind::Existential(alpha_ex_2)), delta))
            }

            _ => unreachable!()
        }
    }
}
