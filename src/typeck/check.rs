use super::{
    ctx::InferCtx,
    ty::{Ex, ExKind, FloatKind, IntKind, MapTy, Ty, TyKind},
    TyResult, Typecker,
};
use crate::{
    dt::idx::IndexVec,
    hir::{
        expr::{ExprKind, Lambda, Lit},
        Expr,
    },
    message::message::MessageBuilder,
    span::{sym::Ident, Spanned, WithSpan},
    typeck::{
        ty::{Adt, ExPair, Field, FieldId, Variant, VariantId},
        TypeckErr,
    },
};

#[derive(Clone, Copy)]
enum InstantiateDir {
    Left,
    Right,
}

impl InstantiateDir {
    fn alternate(&self) -> InstantiateDir {
        match self {
            InstantiateDir::Left => InstantiateDir::Right,
            InstantiateDir::Right => InstantiateDir::Left,
        }
    }
}

impl<'hir> Typecker<'hir> {
    /// Checks if expression is of type `ty` assuming that returned error is
    /// reported.
    pub fn check_discard_err(&mut self, expr_id: Expr, ty: Ty) -> Ty {
        match self.check(expr_id, ty) {
            Ok(ok) => ok,
            Err(err) => {
                err.assert_reported();
                Ty::error()
            },
        }
    }

    // /**
    //  * Checks if expression is of type `ty`.
    //  * If check is successful -- expression node is typed with the type.
    //  */
    // pub fn check_and_type(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
    //     self.check(expr_id, ty).map(|ty| {
    //         self.tyctx_mut().type_node(expr_id, ty);
    //         ty
    //     })
    // }

    /// Checks if expression is of type `ty`.
    /// Reports `Type mismatch` error.
    pub fn check(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        match self._check(expr_id, ty) {
            Ok(ok) => {
                self.tyctx_mut().type_node(expr_id.into(), ty);
                Ok(ok.apply_ctx(self.ctx()))
            },
            Err(_) => {
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

    /// Type check logic starts here.
    fn _check(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        let expr = self.hir.expr(expr_id);

        match (expr.kind(), ty.kind()) {
            (&ExprKind::Lit(lit), check) => {
                match (lit, check) {
                    (Lit::Bool(_), TyKind::Bool) | (Lit::String(_), TyKind::Str) => Ok(ty),
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

    /// Checks if `l_ty` is a subtype of `r_ty` assuming that returned error is
    /// reported.
    pub fn check_ty_discard_err(&mut self, l_ty: Spanned<Ty>, ty: Ty) -> Ty {
        match self.check_ty(l_ty, ty) {
            Ok(ok) => ok,
            Err(err) => {
                err.assert_reported();
                Ty::error()
            },
        }
    }

    /// Checks if `l_ty` is a subtype of `r_ty`.
    /// Reports `Type mismatch` error.
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
    /// Checks if expression's type is a subtype of `ty`.
    fn expr_subtype(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        let expr_ty = self.synth_expr(expr_id)?;

        let span = self.hir.expr(expr_id).span();
        let l = expr_ty.apply_ctx(self.ctx());
        let r = ty.apply_ctx(self.ctx());

        self.subtype(Spanned::new(span, l), r)
    }

    /// Checks if `l_ty` is a subtype of `r_ty`.
    ///
    /// If we've got an error and `l_ty` is an existential, it is solved as an
    /// error. This logic might be invalid and should be verified.
    fn subtype(&mut self, l_ty: Spanned<Ty>, r_ty: Ty) -> TyResult<Ty> {
        match self._subtype(*l_ty.node(), r_ty) {
            Ok(ty) => Ok(ty.apply_ctx(self.ctx())),
            Err(err) => {
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

    /// Subtype logic starts here.
    pub fn _subtype(&mut self, l_ty: Ty, r_ty: Ty) -> TyResult<Ty> {
        assert!(self.ty_wf(l_ty).is_ok());
        assert!(self.ty_wf(r_ty).is_ok());

        match (l_ty.kind(), r_ty.kind()) {
            // FIXME: Is these pats ok?
            (TyKind::Error, _) | (_, TyKind::Error) => Err(TypeckErr::LateReport),

            (TyKind::Unit, TyKind::Unit) => Ok(r_ty),
            (TyKind::Bool, TyKind::Bool) => Ok(r_ty),
            (TyKind::Int(kind), TyKind::Int(kind_)) if kind == kind_ => Ok(r_ty),
            (TyKind::Float(kind), TyKind::Float(kind_)) if kind == kind_ => Ok(r_ty),
            (TyKind::Str, TyKind::Str) => Ok(r_ty),

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
                let params = self._subtype_lists(&params, &params_)?;
                let body1 = body1.apply_ctx(self.ctx());
                let body2 = body2.apply_ctx(self.ctx());
                let body = self._subtype(body1, body2)?;
                Ok(Ty::func(r_ty.func_def_id(), params, body))
                // })
            },

            (&TyKind::Ref(inner), &TyKind::Ref(inner_)) => {
                Ok(Ty::ref_to(self._subtype(inner, inner_)?))
            },

            (&TyKind::Forall(alpha, body), _) => {
                let ex = self.fresh_ex(ExKind::Common);
                let ex_ty = Ty::existential(ex);
                let with_substituted_alpha = body.substitute(alpha, ex_ty);

                self.under_ctx(InferCtx::new_with_ex(ex), |this| {
                    this._subtype(with_substituted_alpha, r_ty)
                })
            },

            (_, &TyKind::Forall(alpha, body)) => self
                .under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this._subtype(l_ty, body)
                }),

            (TyKind::Adt(adt), TyKind::Adt(adt_)) if adt.def_id == adt_.def_id => Ok(r_ty),

            (&TyKind::Existential(ex), _) if ex.is_common() => {
                if !r_ty.contains_ex(ex) {
                    self.instantiate_l(ex, r_ty)
                } else {
                    todo!("Cycle error");
                }
            },

            (_, &TyKind::Existential(ex)) if ex.is_common() => {
                if !l_ty.contains_ex(ex) {
                    self.instantiate_r(l_ty, ex)
                } else {
                    todo!("Cycle error");
                }
            },

            (TyKind::Kind(_), _) | (_, TyKind::Kind(_)) => self.subtype_kind(l_ty, r_ty),

            _ => Err(TypeckErr::LateReport),
        }
    }

    fn _subtype_lists(&mut self, l_tys: &[Ty], r_tys: &[Ty]) -> TyResult<Vec<Ty>> {
        if l_tys.len() != r_tys.len() {
            Err(TypeckErr::LateReport)
        } else {
            l_tys
                .iter()
                .zip(r_tys.iter())
                .map(|(&a, &b)| {
                    if a == b {
                        Ok(b)
                    } else {
                        Err(TypeckErr::LateReport)
                    }
                })
                .collect()
        }
    }

    /// This function is a attempt to generalize logic of left and right
    /// instantiations. It should be rewritten carefully and used.
    #[deprecated]
    fn try_instantiate_common(&mut self, ex: Ex, ty: Ty) -> TyResult<Ty> {
        // Inst(L|R)Reach
        match ty.kind() {
            &TyKind::Existential(ty_ex) => {
                let ex_depth = self.find_unbound_ex_depth(ex);
                let ty_ex_depth = self.find_unbound_ex_depth(ty_ex);

                assert!(self.get_solution(ex).is_none());
                assert!(self.get_solution(ty_ex).is_none());

                if ex_depth <= ty_ex_depth {
                    let ex_ty = Ty::existential(ex);

                    self.solve(ty_ex, ex_ty.mono());
                    return Ok(ex_ty.apply_ctx(self.ctx()));
                }
            },
            _ => {},
        }

        // Inst(L|R)Solve
        if let Some(mono) = ty.as_mono() {
            // FIXME: check WF?
            self.solve(ex, mono);
            return Ok(ty.apply_ctx(self.ctx()));
        }

        Err(TypeckErr::Check)
    }

    /// InstantiateL
    fn instantiate_l(&mut self, ex: Ex, r_ty: Ty) -> TyResult<Ty> {
        if let &TyKind::Existential(beta_ex) = r_ty.kind() {
            if self.find_unbound_ex_depth(ex) < self.find_unbound_ex_depth(beta_ex) {
                return Ok(self.solve(beta_ex, Ty::existential(ex).mono()));
            }
        }

        if let Some(mono) = r_ty.as_mono() {
            return Ok(self.solve(ex, mono));
        }

        match r_ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Var(_)
            | TyKind::Existential(_)
            | TyKind::Ref(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            TyKind::Func(..) | TyKind::FuncDef(..) => {
                self.instantiate_func(InstantiateDir::Left, r_ty, ex)
            },
            &TyKind::Forall(alpha, body) => self.try_to(|this| {
                this.under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this.instantiate_l(ex, body)
                })
            }),

            TyKind::Adt(_) => self.instantiate_adt(InstantiateDir::Left, r_ty, ex),

            TyKind::Kind(_) => unreachable!(),
        }
    }

    /// InstantiateR
    fn instantiate_r(&mut self, l_ty: Ty, ex: Ex) -> TyResult<Ty> {
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
            | TyKind::Str
            | TyKind::Var(_)
            | TyKind::Existential(_)
            | TyKind::Ref(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            TyKind::Func(..) | TyKind::FuncDef(..) => {
                self.instantiate_func(InstantiateDir::Right, l_ty, ex)
            },
            &TyKind::Forall(alpha, body) => self.try_to(|this| {
                let alpha_ex = this.fresh_ex(ExKind::Common);

                this.under_ctx(InferCtx::new_with_ex(alpha_ex), |this| {
                    let alpha_ex_ty = Ty::existential(alpha_ex);
                    let body_ty = body.substitute(alpha, alpha_ex_ty);
                    this.instantiate_r(body_ty, ex)
                })
            }),

            TyKind::Adt(_) => self.instantiate_adt(InstantiateDir::Right, l_ty, ex),

            TyKind::Kind(_) => unreachable!(),
        }
    }

    fn instantiate(&mut self, dir: InstantiateDir, ty: Ty, ex: Ex) -> TyResult<Ty> {
        match dir {
            InstantiateDir::Left => self.instantiate_l(ex, ty),
            InstantiateDir::Right => self.instantiate_r(ty, ex),
        }
    }

    fn instantiate_func(&mut self, dir: InstantiateDir, ty: Ty, ex: Ex) -> TyResult<Ty> {
        let (params, body) = ty.as_func_like();

        self.try_to(|this| {
            let range_ex = this.add_fresh_common_ex();
            let domain_exes = this.add_fresh_common_ex_list(params.len());

            let func_ty = Ty::func(
                ty.func_def_id(),
                domain_exes.iter().copied().map(|(_, ty)| ty).collect(),
                range_ex.1,
            );

            this.solve(ex, func_ty.mono());

            params
                .iter()
                .copied()
                .zip(domain_exes)
                .try_for_each(|(param, param_ex)| {
                    this.instantiate(dir.alternate(), param, param_ex.0)?;
                    Ok(())
                })?;

            let range_ty = body.apply_ctx(this.ctx());
            this.instantiate(dir, range_ty, range_ex.0)
        })
    }

    fn instantiate_adt(&mut self, dir: InstantiateDir, ty: Ty, ex: Ex) -> TyResult<Ty> {
        let adt = ty.as_adt().unwrap();

        self.try_to(|this| {
            let variant_exes = adt
                .variants
                .iter()
                .map(|v| {
                    (
                        v,
                        v.fields
                            .iter()
                            .map(|&f| (f, this.add_fresh_common_ex()))
                            .collect::<IndexVec<FieldId, (Field, ExPair)>>(),
                    )
                })
                .collect::<IndexVec<VariantId, _>>();

            let adt_ex = Ty::adt(Adt {
                def_id: adt.def_id,
                variants: variant_exes
                    .iter()
                    .map(|(v, fields)| Variant {
                        def_id: v.def_id,
                        name: v.name,
                        fields: fields
                            .iter()
                            .map(|(field, (_, ex_ty))| field.map_ty_pure(&mut |_| Ok(*ex_ty)))
                            .collect::<IndexVec<FieldId, _>>(),
                    })
                    .collect(),
            });

            this.solve(ex, adt_ex.mono());

            adt.variants.iter_enumerated().try_for_each(|(vid, v)| {
                v.fields.iter_enumerated().try_for_each(|(fid, f)| {
                    this.instantiate(dir.alternate(), f.ty, variant_exes[vid].1[fid].1 .0)?;
                    Ok(())
                })?;
                Ok(())
            })?;

            Ok(ty)
        })
    }
}
