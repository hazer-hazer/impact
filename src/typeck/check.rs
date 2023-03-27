use crate::{
    cli::verbose,
    hir::expr::{Expr, ExprKind, Lambda, Lit},
    message::message::MessageBuilder,
    span::span::{Ident, Spanned, WithSpan},
    typeck::{ty::Subst, TypeckErr},
};

use super::{
    ctx::InferCtx,
    ty::{Existential, ExistentialKind, FloatKind, IntKind, Ty, TyKind},
    TyResult,
};

use super::Typecker;

impl<'hir> Typecker<'hir> {
    /**
     * Checks if expression is of type `ty` assuming that returned error is reported.
     */
    pub fn check_discard_err(&mut self, expr_id: Expr, ty: Ty) -> Ty {
        match self.check(expr_id, ty) {
            Ok(ok) => ok,
            Err(err) => {
                err.assert_reported();
                Ty::error()
            },
        }
    }

    /**
     * Checks if expression is of type `ty`.
     * Reports `Type mismatch` error.
     */
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

    /**
     * Type check logic starts here.
     */
    fn _check(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        let expr = self.hir.expr(expr_id);

        match (expr.kind(), ty.kind()) {
            (&ExprKind::Lit(lit), check) => {
                match (lit, check) {
                    (Lit::Bool(_), TyKind::Bool) | (Lit::String(_), TyKind::String) => Ok(ty),
                    (Lit::Int(_, ast_kind), &TyKind::Int(kind)) => IntKind::try_from(ast_kind)
                        .map_or_else(
                            |_| self.expr_subtype(expr_id, ty),
                            |kind_| {
                                if kind == kind_ {
                                    Ok(ty)
                                } else {
                                    Err(TypeckErr::LateReport)
                                }
                            },
                        ),

                    (Lit::Float(_, ast_kind), &TyKind::Float(kind)) => {
                        FloatKind::try_from(ast_kind).map_or_else(
                            |_| self.expr_subtype(expr_id, ty),
                            |kind_| {
                                if kind == kind_ {
                                    Ok(ty)
                                } else {
                                    Err(TypeckErr::LateReport)
                                }
                            },
                        )
                    },

                    // Unequal literals (not existentials as we not failed
                    //  to construct PrimTy of Lit without context)
                    _ => Err(TypeckErr::LateReport),
                }
            },

            (
                &ExprKind::Lambda(Lambda { body_id: body, .. }),
                TyKind::FuncDef(_, params_tys, body_ty),
            ) => {
                let param_names = self
                    .hir
                    .body(body)
                    .params
                    .iter()
                    .filter_map(|param| self.hir.pat_names(*param))
                    .flatten()
                    .collect::<Vec<Ident>>();

                self.under_ctx(
                    InferCtx::new_with_term_map(&param_names, &params_tys),
                    |this| this._check(self.hir.body(body).value, *body_ty),
                )
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

    /**
     * Checks if `l_ty` is a subtype of `r_ty` assuming that returned error is reported.
     */
    pub fn check_ty_discard_err(&mut self, l_ty: Spanned<Ty>, ty: Ty) -> Ty {
        match self.check_ty(l_ty, ty) {
            Ok(ok) => ok,
            Err(err) => {
                err.assert_reported();
                Ty::error()
            },
        }
    }

    /**
     * Checks if `l_ty` is a subtype of `r_ty`.
     * Reports `Type mismatch` error.
     */
    pub fn check_ty(&mut self, l_ty: Spanned<Ty>, r_ty: Ty) -> TyResult<Ty> {
        match self.subtype(l_ty, r_ty) {
            Ok(ok) => Ok(ok),
            Err(_) => {
                let span = l_ty.span();
                let l_ty = l_ty.node();

                MessageBuilder::error()
                    .span(span)
                    .text(format!("{} is not a subtype of {}", l_ty, r_ty))
                    .label(span, format!("Must be of type {}", r_ty))
                    .emit(self);

                Err(TypeckErr::Reported)
            },
        }
    }

    // Subtyping //
    /**
     * Checks if expression's type is a subtype of `ty`.
     */
    fn expr_subtype(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        verbose!("Subtype expr {} / {}", expr_id, ty);

        let expr_ty = self.synth_expr(expr_id)?;

        verbose!("Subtype expr ty: {}", expr_ty);

        let span = self.hir.expr(expr_id).span();
        let l = self.apply_ctx_on(expr_ty);
        let r = self.apply_ctx_on(ty);

        self.subtype(Spanned::new(span, l), r)
    }

    /**
     * Checks if `l_ty` is a subtype of `r_ty`.
     *
     * If we've got an error and `l_ty` is an existential, it is solved as an error.
     * This logic might be invalid and should be verified.
     */
    fn subtype(&mut self, l_ty: Spanned<Ty>, r_ty: Ty) -> TyResult<Ty> {
        match self._subtype(*l_ty.node(), r_ty) {
            Ok(ty) => {
                verbose!("[+] {} is a subtype of {}", l_ty.node(), ty);
                Ok(self.apply_ctx_on(ty))
            },
            Err(err) => {
                verbose!("[-] {} is NOT a subtype of {}", l_ty.node(), r_ty);

                // let span = check_ty.span();
                // let l_ty = *check_ty.node();

                // MessageBuilder::error()
                //     .span(span)
                //     .text(format!("{} is not a subtype of {}", l_ty, r_ty))
                //     .label(span, format!("{} is not a subtype of {}", l_ty, r_ty))
                //     .emit(self);

                // TODO: Check this logic
                match l_ty.node().kind() {
                    &TyKind::Existential(ex) => {
                        self.solve(ex, Ty::error().mono());
                    },
                    _ => {},
                }

                // Err(TypeckErr::Reported)
                Err(err)
            },
        }
    }

    /**
     * Subtype logic starts here.
     */
    fn _subtype(&mut self, l_ty: Ty, r_ty: Ty) -> TyResult<Ty> {
        verbose!(
            "Subtype {} <: {}",
            self.apply_ctx_on(l_ty),
            self.apply_ctx_on(r_ty)
        );

        assert!(self.ty_wf(l_ty).is_ok());
        assert!(self.ty_wf(r_ty).is_ok());

        match (l_ty.kind(), r_ty.kind()) {
            // FIXME: Is these pats ok?
            (TyKind::Error, _) | (_, TyKind::Error) => Err(TypeckErr::LateReport),

            (TyKind::Unit, TyKind::Unit) => Ok(r_ty),
            (TyKind::Bool, TyKind::Bool) => Ok(r_ty),
            (TyKind::Int(kind), TyKind::Int(kind_)) if kind == kind_ => Ok(r_ty),
            (TyKind::Float(kind), TyKind::Float(kind_)) if kind == kind_ => Ok(r_ty),
            (TyKind::String, TyKind::String) => Ok(r_ty),

            (TyKind::Var(var), TyKind::Var(var_)) if var == var_ => Ok(r_ty),

            (TyKind::Existential(ex1), TyKind::Existential(ex2)) if ex1 == ex2 => {
                self.ty_wf(l_ty).and(self.ty_wf(r_ty))
            },

            // Int existentials //
            (&TyKind::Existential(int_ex), TyKind::Int(_)) if int_ex.is_int() => {
                Ok(self.solve(int_ex, r_ty.mono()))
            },

            (&TyKind::Existential(int_ex), &TyKind::Existential(ex)) if int_ex.is_int() => {
                // FIXME: This is a test logic
                Ok(self.solve(ex, Ty::default_int().mono()))
            },

            (TyKind::Existential(int_ex), _) if int_ex.is_int() => Err(TypeckErr::LateReport),

            // Float existentials //
            (&TyKind::Existential(float_ex), TyKind::Float(_)) if float_ex.is_float() => {
                Ok(self.solve(float_ex, r_ty.mono()))
            },

            (TyKind::Existential(float_ex), _) if float_ex.is_float() => Err(TypeckErr::LateReport),

            //
            (TyKind::Func(params, body1), TyKind::Func(params_, body2))
            | (TyKind::FuncDef(_, params, body1), TyKind::Func(params_, body2))
            | (TyKind::Func(params, body1), TyKind::FuncDef(_, params_, body2))
            | (TyKind::FuncDef(_, params, body1), TyKind::FuncDef(_, params_, body2)) => {
                // self.under_new_ctx(|this| {
                // Enter Θ
                self._subtype_lists(&params, &params_)?;
                let body1 = self.apply_ctx_on(*body1);
                let body2 = self.apply_ctx_on(*body2);
                self._subtype(body1, body2)
                // })
            },

            (&TyKind::Forall(alpha, body), _) => {
                verbose!("forall {}. {} subtype of (?)", alpha, body);
                let ex = self.fresh_ex(ExistentialKind::Common);
                let ex_ty = Ty::existential(ex);
                let with_substituted_alpha = body.substitute(Subst::Var(alpha), ex_ty);

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

    fn _subtype_lists(&mut self, l_tys: &[Ty], r_tys: &[Ty]) -> TyResult<()> {
        if l_tys.len() != r_tys.len() {
            Err(TypeckErr::LateReport)
        } else {
            l_tys
                .iter()
                .zip(r_tys.iter())
                .all(|(a, b)| a == b)
                .then_some(())
                .ok_or(TypeckErr::LateReport)
        }
    }

    /**
     * This function is a attempt to generalize logic of left and right instantiations.
     * It should be rewritten carefully and used.
     */
    #[deprecated]
    fn try_instantiate_common(&mut self, ex: Existential, ty: Ty) -> TyResult<Ty> {
        // Inst(L|R)Reach
        match ty.kind() {
            &TyKind::Existential(ty_ex) => {
                let ex_depth = self.find_unbound_ex_depth(ex);
                let ty_ex_depth = self.find_unbound_ex_depth(ty_ex);

                assert!(self.get_solution(ex).is_none());
                assert!(self.get_solution(ty_ex).is_none());

                verbose!(
                    "Existential depth {:?} /vs/ ty depth {:?}",
                    ex_depth,
                    ty_ex_depth
                );
                if ex_depth <= ty_ex_depth {
                    let ex_ty = Ty::existential(ex);

                    verbose!("Instantiate L|R Reach {} = {}", ty_ex, ex_ty);

                    self.solve(ty_ex, ex_ty.mono());
                    return Ok(self.apply_ctx_on(ex_ty));
                }
            },
            _ => {},
        }

        // Inst(L|R)Solve
        if let Some(mono) = ty.as_mono() {
            verbose!("Instantiate L|R Solve {} = {}", ex, ty);
            // FIXME: check WF?
            self.solve(ex, mono);
            return Ok(self.apply_ctx_on(ty));
        }

        Err(TypeckErr::Check)
    }

    /**
     * InstantiateL
     */
    fn instantiate_l(&mut self, ex: Existential, r_ty: Ty) -> TyResult<Ty> {
        verbose!("Instantiate Left {} / {}", ex, r_ty);

        if let &TyKind::Existential(beta_ex) = r_ty.kind() {
            if self.find_unbound_ex_depth(ex) < self.find_unbound_ex_depth(beta_ex) {
                verbose!("InstLReach: ");
                return Ok(self.solve(beta_ex, Ty::existential(ex).mono()));
            }
        }

        if let Some(mono) = r_ty.as_mono() {
            verbose!("InstLSolve: ");
            return Ok(self.solve(ex, mono));
        }

        match r_ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::String
            | TyKind::Var(_)
            | TyKind::Existential(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => self.try_to(|this| {
                let range_ex = this.add_fresh_common_ex();
                let domain_exes = this.add_fresh_common_ex_list(params.len());

                let func_ty = Ty::func(
                    r_ty.func_def_id(),
                    domain_exes.iter().copied().map(|(_, ty)| ty).collect(),
                    range_ex.1,
                );

                this.solve(ex, func_ty.mono());

                params
                    .iter()
                    .copied()
                    .enumerate()
                    .try_for_each(|(index, param)| {
                        this.instantiate_r(param, domain_exes[index].0)?;
                        Ok(())
                    })?;

                let range_ty = this.apply_ctx_on(*body);
                this.instantiate_l(range_ex.0, range_ty)
            }),
            &TyKind::Forall(alpha, body) => self.try_to(|this| {
                this.under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this.instantiate_l(ex, body)
                })
            }),
        }
    }

    /**
     * InstantiateR
     */
    fn instantiate_r(&mut self, l_ty: Ty, ex: Existential) -> TyResult<Ty> {
        verbose!("Instantiate Right {} / {}", ex, l_ty);

        if let &TyKind::Existential(beta_ex) = l_ty.kind() {
            if self.find_unbound_ex_depth(ex) < self.find_unbound_ex_depth(beta_ex) {
                return Ok(self.solve(beta_ex, Ty::existential(ex).mono()));
            }
        }

        if let Some(mono) = l_ty.as_mono() {
            return Ok(self.solve(ex, mono));
        }

        match l_ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::String
            | TyKind::Var(_)
            | TyKind::Existential(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => self.try_to(|this| {
                let range_ex = this.add_fresh_common_ex();
                let domain_exes = this.add_fresh_common_ex_list(params.len());

                let func_ty = Ty::func(
                    l_ty.func_def_id(),
                    domain_exes.iter().copied().map(|(_, ty)| ty).collect(),
                    range_ex.1,
                );

                this.solve(ex, func_ty.mono());

                params
                    .iter()
                    .copied()
                    .enumerate()
                    .try_for_each(|(index, param)| {
                        this.instantiate_l(domain_exes[index].0, param)?;
                        Ok(())
                    })?;

                let range_ty = this.apply_ctx_on(*body);
                this.instantiate_r(range_ty, range_ex.0)
            }),
            &TyKind::Forall(alpha, body) => self.try_to(|this| {
                verbose!("Instantiate forall Right {} = {}", ex, l_ty);

                let alpha_ex = this.fresh_ex(ExistentialKind::Common);

                this.under_ctx(InferCtx::new_with_ex(alpha_ex), |this| {
                    let alpha_ex_ty = Ty::existential(alpha_ex);
                    let body_ty = body.substitute(Subst::Var(alpha), alpha_ex_ty);
                    this.instantiate_r(body_ty, ex)
                })
            }),
        }
    }
}
