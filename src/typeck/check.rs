use crate::{
    cli::verbose,
    hir::expr::{Expr, ExprKind},
    message::message::MessageBuilder,
    span::span::{Spanned, WithSpan},
    typeck::{ty::Subst, TypeckErr},
};

use super::{
    ctx::InferCtx,
    ty::{Existential, ExistentialKind, PrimTy, Ty, TyKind},
    TyResult,
};

use super::Typecker;

impl<'hir> Typecker<'hir> {
    pub fn check(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        match self._check(expr_id, ty) {
            Ok(ok) => {
                verbose!("[+] Expr {} is of type {}", expr_id, ty);
                Ok(self.apply_ctx_on(ok))
            },
            Err(_) => {
                verbose!("[-] Expr {} is NOT of type {}", expr_id, ty);

                let span = self.hir.expr_result_span(expr_id);

                MessageBuilder::error()
                    .span(span)
                    .text(format!(
                        "Type mismatch: expected {}{}",
                        ty,
                        if let Some(got) = self.tyctx().node_type(expr_id) {
                            format!(", got {}", got)
                        } else {
                            "".to_string()
                        }
                    ))
                    .label(span, format!("Must be of type {}", ty))
                    .emit(self);

                Err(TypeckErr::Reported)
            },
        }
    }

    fn _check(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        let expr = self.hir.expr(expr_id);

        match (expr.kind(), ty.kind()) {
            (&ExprKind::Lit(lit), &TyKind::Prim(prim)) => {
                if let Ok(lit_prim) = PrimTy::try_from(lit) {
                    if lit_prim == prim {
                        Ok(ty)
                    } else {
                        // Unequal literals (not existentials as we not failed
                        //  to construct PrimTy of Lit without context)
                        Err(TypeckErr::LateReport)
                    }
                } else {
                    // Infer literal ty
                    self.expr_subtype(expr_id, ty)
                }
            },

            (ExprKind::Lambda(lambda), &TyKind::Func(param_ty, body_ty)) => {
                let param_name = self.hir.pat_names(lambda.param).unwrap();
                assert!(param_name.len() == 1);
                let param_name = param_name[0];

                self.under_ctx(InferCtx::new_with_term(param_name, param_ty), |this| {
                    this._check(lambda.body, body_ty)
                })
            },

            (_, &TyKind::Forall(alpha, body)) => {
                self.under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this._check(expr_id, body)?;
                    Ok(ty)
                })
            },

            _ => self.expr_subtype(expr_id, ty),
        }
    }

    // Subtyping //
    fn expr_subtype(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        let expr = self.hir.expr(expr_id);

        let expr_ty = self.synth_expr(expr_id)?;
        let l = self.apply_ctx_on(expr_ty);
        let r = self.apply_ctx_on(ty);

        self.subtype(Spanned::new(expr.span(), l), r)
    }

    fn subtype(&mut self, check_ty: Spanned<Ty>, r_ty: Ty) -> TyResult<Ty> {
        match self._subtype(*check_ty.node(), r_ty) {
            Ok(ty) => {
                verbose!("[+] {} is a subtype of {}", check_ty.node(), ty);
                Ok(self.apply_ctx_on(ty))
            },
            Err(err) => {
                verbose!("[-] {} is NOT a subtype of {}", check_ty.node(), r_ty);

                // let span = check_ty.span();
                // let l_ty = *check_ty.node();

                // MessageBuilder::error()
                //     .span(span)
                //     .text(format!("{} is not a subtype of {}", l_ty, r_ty))
                //     .label(span, format!("{} is not a subtype of {}", l_ty, r_ty))
                //     .emit(self);

                // TODO: Check this logic
                match check_ty.node().kind() {
                    &TyKind::Existential(ex) => {
                        self.solve(ex, Ty::error());
                    },
                    _ => {},
                }

                // Err(TypeckErr::Reported)
                Err(err)
            },
        }
    }

    fn _subtype(&mut self, l_ty: Ty, r_ty: Ty) -> TyResult<Ty> {
        assert!(self.ty_wf(l_ty).is_ok());
        assert!(self.ty_wf(r_ty).is_ok());

        match (l_ty.kind(), r_ty.kind()) {
            (&TyKind::Prim(prim), &TyKind::Prim(prim_)) if prim == prim_ => Ok(r_ty),

            (TyKind::Var(name1), TyKind::Var(name2)) if name1.sym() == name2.sym() => Ok(r_ty),

            (TyKind::Existential(ex1), TyKind::Existential(ex2)) if ex1 == ex2 => {
                self.ty_wf(l_ty).and(self.ty_wf(r_ty))
            },

            (&TyKind::Existential(int_ex), TyKind::Prim(PrimTy::Int(_))) if int_ex.is_int() => {
                Ok(self.solve(int_ex, r_ty))
            },

            (TyKind::Existential(int_ex), _) if int_ex.is_int() => Err(TypeckErr::LateReport),

            (&TyKind::Existential(float_ex), TyKind::Prim(PrimTy::Float(_)))
                if float_ex.is_float() =>
            {
                Ok(self.solve(float_ex, r_ty))
            },

            (TyKind::Existential(float_ex), _) if float_ex.is_float() => Err(TypeckErr::LateReport),

            (&TyKind::Func(param1, body1), &TyKind::Func(param2, body2)) => {
                // self.under_new_ctx(|this| {
                // Enter Θ
                self._subtype(param1, param2)?;
                let body1 = self.apply_ctx_on(body1);
                let body2 = self.apply_ctx_on(body2);
                self._subtype(body1, body2)
                // })
            },

            (&TyKind::Forall(alpha, body), _) => {
                verbose!("forall {}. {} subtype of (?)", alpha, body);
                let ex = self.fresh_ex(ExistentialKind::Common);
                let ex_ty = Ty::existential(ex);
                let with_substituted_alpha = self.substitute(body, Subst::Name(alpha), ex_ty);

                self.under_ctx(InferCtx::new_with_ex(ex), |this| {
                    this._subtype(with_substituted_alpha, r_ty)
                })
            },

            (_, &TyKind::Forall(alpha, body)) => self
                .under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this._subtype(l_ty, body)
                }),

            (&TyKind::Existential(ex), _) if ex.is_common() => {
                if !self.ty_occurs_in(r_ty, Subst::Existential(ex)) {
                    self.instantiate_l(ex, r_ty)
                } else {
                    todo!("Cycle error");
                }
            },

            (_, &TyKind::Existential(ex)) if ex.is_common() => {
                if !self.ty_occurs_in(l_ty, Subst::Existential(ex)) {
                    self.instantiate_r(l_ty, ex)
                } else {
                    todo!("Cycle error");
                }
            },

            _ => Err(TypeckErr::LateReport),
        }
    }

    fn try_instantiate_common(&mut self, ex: Existential, ty: Ty) -> TyResult<Ty> {
        // Inst(L|R)Reach
        match ty.kind() {
            &TyKind::Existential(ty_ex) => {
                let ex_depth = self.find_unbound_ex_depth(ex);
                let ty_ex_depth = self.find_unbound_ex_depth(ty_ex);
                if ex_depth <= ty_ex_depth {
                    let ex_ty = Ty::existential(ex);

                    verbose!("Instantiate L|R Reach {} = {}", ty_ex, ex_ty);

                    self.solve(ty_ex, ex_ty);
                    return Ok(self.apply_ctx_on(ex_ty));
                }
            },
            _ => {},
        }

        // Inst(L|R)Solve
        if ty.is_mono() {
            verbose!("Instantiate L|R Solve {} = {}", ex, ty);
            // FIXME: check WF?
            self.solve(ex, ty);
            return Ok(self.apply_ctx_on(ty));
        }

        Err(TypeckErr::Check)
    }

    fn instantiate_l(&mut self, ex: Existential, r_ty: Ty) -> TyResult<Ty> {
        if let Ok(ok) = self.try_instantiate_common(ex, r_ty) {
            return Ok(ok);
        }

        verbose!("Instantiate Left {} = {}", ex, r_ty);

        match r_ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Prim(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            &TyKind::Func(param, body) => self.try_to(|this| {
                let range_ex = this.add_fresh_common_ex();
                let domain_ex = this.add_fresh_common_ex();

                let func_ty = Ty::func(domain_ex.1, range_ex.1);

                this.solve(ex, func_ty);

                this.instantiate_r(param, domain_ex.0)?;

                let range_ty = this.apply_ctx_on(body);
                this.instantiate_l(range_ex.0, range_ty)
            }),
            &TyKind::Forall(alpha, body) => self.try_to(|this| {
                this.under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this.instantiate_l(ex, body)
                })
            }),
        }
    }

    fn instantiate_r(&mut self, l_ty: Ty, ex: Existential) -> TyResult<Ty> {
        if let Ok(ok) = self.try_instantiate_common(ex, l_ty) {
            return Ok(ok);
        }

        verbose!("Instantiate Right {} = {}", ex, l_ty);

        match l_ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Prim(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            &TyKind::Func(param, body) => self.try_to(|this| {
                let domain_ex = this.add_fresh_common_ex();
                let range_ex = this.add_fresh_common_ex();

                let func_ty = Ty::func(domain_ex.1, range_ex.1);

                this.solve(ex, func_ty);

                this.instantiate_l(domain_ex.0, param)?;

                let range_ty = this.apply_ctx_on(body);
                this.instantiate_r(range_ty, range_ex.0)
            }),
            &TyKind::Forall(alpha, body) => self.try_to(|this| {
                verbose!("Instantiate forall Right {} = {}", ex, l_ty);

                let alpha_ex = this.fresh_ex(ExistentialKind::Common);

                this.under_ctx(InferCtx::new_with_ex(alpha_ex), |this| {
                    let alpha_ex_ty = Ty::existential(alpha_ex);
                    let body_ty = this.substitute(body, Subst::Name(alpha), alpha_ex_ty);
                    this.instantiate_r(body_ty, ex)
                })
            }),
        }
    }
}
