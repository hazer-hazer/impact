use super::{
    ty::{ExKind, FloatKind, IntKind, Struct, Ty, TyKind, VariantData},
    TyResult, TyResultImpl, TypeckErr, Typecker, Typed,
};
use crate::{
    cli::verbose,
    dt::idx::IndexVec,
    hir::{
        self,
        expr::{Arm, Call, ExprKind, Lit, TyExpr},
        item::{ItemId, ItemKind, Mod},
        stmt::{Local, StmtKind},
        Block, Body, BodyId, Expr, ExprPath, ExprRes, Map, Pat, Stmt, ValueDefKind,
    },
    message::message::MessageBuilder,
    resolve::def::{DefId, DefKind},
    session::{MaybeWithSession, SessionHolder},
    span::{sym::Ident, Spanned, WithSpan},
    typeck::{
        debug::{tcdbg, InferEntryKind, InferStepKind},
        kind::{Kind, KindSort},
        ty::{Field, FieldId},
    },
};

impl<'hir> Typecker<'hir> {
    pub fn synth_item(&mut self, item_id: ItemId) -> TyResult<Ty> {
        let hir_id = item_id.hir_id();
        match self.sess.def_table.def(item_id.def_id()).kind() {
            DefKind::DeclareBuiltin => {
                self.type_inferring_node(
                    hir_id,
                    Ty::func(
                        Some(item_id.def_id()),
                        vec![Ty::str()],
                        Ty::var(Ty::next_ty_var_id(None)),
                    ),
                );
                return Ok(Ty::unit());
            },
            _ => {},
        }

        let item = self.hir.item(item_id);

        let ty = match item.kind() {
            ItemKind::TyAlias(_ty) => {
                verbose!("Synth ty alias {}", item.def_id());
                // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                return Ok(self.tyctx().node_type(hir_id).unwrap());
            },
            ItemKind::Mod(Mod { items, .. }) => {
                // FIXME: How not to clone?
                for item in items.clone() {
                    self.synth_item(item)?;
                }
                Ty::unit()
            },
            // Note: Actually, declaration type is a unit type, but we save it
            // TODO: Add encapsulation layer such as `get_def_ty` (with closed access to
            // TyCtx::typed) which will check if definition CAN have a type
            &ItemKind::Value(body) | &ItemKind::Func(body) => {
                let value_ty = self.synth_body(item.def_id(), body)?;
                let conv_def_ty = self
                    .tyctx()
                    .def_ty(item_id.def_id())
                    .unwrap()
                    .as_kind_ex()
                    .unwrap();
                self.solve_kind_ex(conv_def_ty, Kind::new_ty(value_ty).mono());
                value_ty
            },
            ItemKind::ExternItem(extern_item) => {
                let ty = self
                    .tyctx()
                    .get_conv(extern_item.ty)
                    .unwrap()
                    .maybe_add_func_def_id(item.def_id());
                ty
            },
            ItemKind::Adt(_) => self.tyctx().node_type(hir_id).unwrap(),
            ItemKind::Struct(_) => self.tyctx().node_type(hir_id).unwrap(),
        };

        let ty = ty.apply_ctx(self);
        self.type_inferring_node(hir_id, ty);

        TyResult::Ok(Ty::unit())
    }

    fn synth_stmt(&mut self, stmt: Stmt) -> TyResult<Ty> {
        let stmt = self.hir.stmt(stmt);

        match stmt.kind() {
            &StmtKind::Expr(expr) => {
                self.synth_expr(expr)?;
                // Expression statement is always an unused result, thus we need to default all
                // number existentials
                self.default_number_exes();
            },
            &StmtKind::Item(item) => {
                self.synth_item(item)?;
            },
            StmtKind::Local(local) => {
                self.synth_local_stmt(local)?;
            },
        }

        Ok(Ty::unit())
    }

    fn synth_local_stmt(&mut self, local: &Local) -> TyResult<Ty> {
        let pat_ty = self.synth_pat(local.pat)?;

        // FIXME: Do pattern check
        let body_ty = self.synth_expr(local.value)?.apply_ctx(self);

        self.subtype(
            Spanned::new(self.hir.pat(local.pat).span(), pat_ty),
            body_ty,
        )?;

        Ok(Ty::unit())
    }

    pub fn synth_expr(&mut self, expr: Expr) -> TyResult<Ty> {
        let ie = tcdbg!(self, enter InferEntryKind::ForExpr(expr));

        let expr_ty = match self.hir.expr(expr).kind() {
            ExprKind::Lit(lit) => self.synth_lit(&lit),
            &ExprKind::Path(path) => self.synth_path(path),
            &ExprKind::Block(block) => self.synth_block(block),
            ExprKind::Lambda(lambda) => self.synth_body(lambda.def_id, lambda.body_id),
            ExprKind::Call(call) => self.synth_call(&call, expr),
            &ExprKind::Let(block) => self.under_ctx(|this| this.synth_block(block)),
            ExprKind::Ty(ty_expr) => self.synth_ty_expr(&ty_expr),
            // &ExprKind::FieldAccess(lhs, field) => self.synth_field_access_expr(lhs, field,
            // expr_id),
            &ExprKind::Builtin(bt) => Ok(self.tyctx().builtin(bt.into())),
            ExprKind::Match(subject, arms) => self.synth_match_expr(subject, arms),
        }?;

        tcdbg!(self, step InferStepKind::Synthesized(expr_ty));

        let ty = expr_ty.apply_ctx(self);

        tcdbg!(self, step InferStepKind::CtxApplied(expr_ty, ty));

        self.type_inferring_node(expr, ty);

        tcdbg!(self, exit ie);

        Ok(ty)
    }

    fn synth_lit(&mut self, lit: &Lit) -> TyResult<Ty> {
        Ok(match lit {
            Lit::Bool(_) => Ty::bool(),
            Lit::String(_) => Ty::str(),

            &Lit::Int(_, kind) => IntKind::try_from(kind)
                .map_or_else(|_| self.add_fresh_ex(ExKind::Int).1, |kind| Ty::int(kind)),
            &Lit::Float(_, kind) => FloatKind::try_from(kind).map_or_else(
                |_| self.add_fresh_ex(ExKind::Float).1,
                |kind| Ty::float(kind),
            ),
        })
    }

    // Note: This is a path **expression**
    fn synth_path(&mut self, path: ExprPath) -> TyResult<Ty> {
        self.synth_res(self.hir.expr_path(path).res())
    }

    fn synth_res(&mut self, res: &ExprRes) -> TyResult<Ty> {
        match res {
            &ExprRes::Def(def_kind, def_id) => match def_kind {
                // Definition types collected in `conv`
                ValueDefKind::External
                | ValueDefKind::FieldAccessor
                | ValueDefKind::Ctor
                | ValueDefKind::Func
                | ValueDefKind::Value => Ok(self.tyctx().def_ty(def_id).expect(&format!(
                    "Expected type of {} definition to be collected at conv stage",
                    self.sess.def_table.def(def_id)
                ))),
            },
            &ExprRes::Local(local) => Ok(self.tyctx().node_type(local).unwrap()),
        }
    }

    fn synth_ty_expr(&mut self, ty_expr: &TyExpr) -> TyResult<Ty> {
        // FIXME: Check wf?
        // FIXME: Do we need `try_to` here?
        self.try_to(|this| {
            let anno_ty = this.tyctx().get_conv(ty_expr.ty).unwrap();
            this.check(ty_expr.expr, anno_ty)
        })
    }

    fn synth_match_expr(&mut self, &subject: &Expr, arms: &[Arm]) -> TyResult<Ty> {
        let subject_node = self.hir.expr(subject);
        let subject_ty = self.synth_expr(subject)?;
        let arms_tys = arms
            .iter()
            .map(|arm| Ok(self.synth_arm(subject_ty, arm)?))
            .collect::<Result<Vec<_>, _>>()?;
        if let Some(&first_arm_ty) = arms_tys.first() {
            if !arms_tys
                .iter()
                .copied()
                .all(|arm_ty| arm_ty == first_arm_ty)
            {
                MessageBuilder::error()
                    .span(self.hir.expr(arms.first().unwrap().body).span())
                    .text(format!("Match arms have incompatible types"))
                    .label_iter(arms_tys.iter().zip(arms.iter()).map(|(arm_ty, arm)| {
                        (
                            self.hir.expr(arm.body).span(),
                            format!("is {}", arm_ty.with_sess(self.sess())),
                        )
                    }))
                    .emit(self);
            }

            // Note: Even if match arms have incompatible types, we synthesize match type as
            // first arm type
            Ok(first_arm_ty)
        } else {
            MessageBuilder::error()
                .span(subject_node.span())
                .text(format!(
                    "Not all possible values of match value with type {} handled",
                    subject_ty.with_sess(self.sess())
                ))
                .emit_single_label(self);
            Err(TypeckErr::Reported)
        }
    }

    fn synth_arm(&mut self, subject_ty: Ty, arm: &Arm) -> TyResult<Ty> {
        let pat_ty = self.synth_pat(arm.pat)?;
        // // TODO: Synth optional type of pattern
        let expr_ty = self.synth_expr(arm.body)?;

        self.subtype(
            Spanned::new(self.hir.pat(arm.pat).span(), pat_ty),
            subject_ty,
        )?;

        Ok(expr_ty)
    }

    fn synth_block(&mut self, block: Block) -> TyResult<Ty> {
        let block = self.hir.block(block);

        self.under_ctx(|this| {
            block.stmts().iter().for_each(|&stmt| {
                let _ignore_stmt_typeck_err = this.synth_stmt(stmt);
            });

            let res_ty = block
                .expr()
                .map_or(Ok(Ty::unit()), |expr| this.synth_expr(expr));

            // FIXME: Can we live without this?
            // this.default_number_exes();
            // this.verify_ctx();

            res_ty
        })
    }

    /// Synthesize pattern virgin type (before usage)
    // TODO: Annotations
    pub fn synth_pat(&mut self, pat: Pat) -> TyResult<Ty> {
        let kind_ty = match self.hir.pat(pat).kind() {
            hir::pat::PatKind::Unit => Ty::unit(),
            &hir::pat::PatKind::Ident(_, name_id) => {
                let name_ex = self.add_fresh_kind_ex_as_ty().1;
                self.type_inferring_node(name_id, name_ex);
                name_ex
            },
            hir::pat::PatKind::Struct(ty_path, pat_fields, rest) => {
                let struct_res = self.hir.ty_path(*ty_path).res().def_id();
                let pat_ty = self.tyctx().def_ty(struct_res).unwrap();

                let pat_struct_ty = pat_ty.as_struct().unwrap();

                // Convert fields to canonical order
                let synthed_fields = if let Some(names) = pat_struct_ty.data.names_indices() {
                    pat_fields.iter().fold(
                        IndexVec::with_capacity(pat_struct_ty.data.fields.len()),
                        |mut ty_fields, field| {
                            let name =
                                field.name.or_else(|| match self.hir.pat(field.pat).kind() {
                                    &hir::pat::PatKind::Ident(name, _) => Some(name),
                                    _ => None,
                                });

                            let field_pat = self.hir.pat(field.pat);
                            let field_pat_ty = self.synth_pat(field.pat);
                            if let Some(name) = name {
                                if let Some(field_id) = names.get(&name) {
                                    ty_fields[*field_id] = Some((field_pat_ty, field.pat));
                                } else {
                                    MessageBuilder::error()
                                        .text(format!(
                                            "Struct `{}` does not contain field named `{name}`",
                                            self.hir.ty_path(*ty_path).target_name()
                                        ))
                                        .span(name.span())
                                        .emit_single_label(self);
                                }
                            } else {
                                MessageBuilder::error()
                                    .text(format!("Expected field name"))
                                    .span(field_pat.span().point_before_lo())
                                    .label(
                                        field_pat.span().point_before_lo(),
                                        format!("Add field name here"),
                                    )
                                    .emit(self);
                            }

                            ty_fields
                        },
                    )
                } else {
                    pat_fields
                        .iter()
                        .map(|field| {
                            assert!(field.name.is_none());
                            Some((self.synth_pat(field.pat), field.pat))
                        })
                        .collect::<IndexVec<FieldId, _>>()
                };

                let missing_fields_span = self.hir.ty_path(*ty_path).span().point_after_hi();
                let inferred_fields = pat_struct_ty
                    .data
                    .fields
                    .iter()
                    .zip(synthed_fields)
                    .map(|(ty_field, pat_field)| {
                        if let Some((pat_field_ty, pat_field)) = pat_field {
                            Field {
                                name: ty_field.name,
                                ty: self
                                    .subtype(
                                        Spanned::new(
                                            self.hir.pat(pat_field).span(),
                                            pat_field_ty.unwrap_as_ty(),
                                        ),
                                        ty_field.ty,
                                    )
                                    .unwrap_as_ty(),
                            }
                        } else if *rest {
                            ty_field.clone()
                        } else {
                            MessageBuilder::error()
                                .span(missing_fields_span)
                                .text(format!("Missing field"))
                                .emit_single_label(self);
                            ty_field.clone()
                        }
                    })
                    .collect();

                Ty::struct_(Struct {
                    data: VariantData {
                        def_id: pat_struct_ty.data.def_id,
                        fields: inferred_fields,
                    },
                })
            },
            &hir::pat::PatKind::Or(lpat, rpat) => {
                let pat_ty = self.synth_pat(lpat)?;
                self.synth_pat(rpat)?;
                pat_ty
            },
        };

        self.type_inferring_node(pat, kind_ty);

        Ok(kind_ty)
    }

    /// Return list of types and pattern names. This is used for replacement of
    /// unsolved existentials with universally quantifiers, and names are just
    /// for readability to name universally quantified variables as their
    /// pattern names.
    fn get_pat_inner_tys(&self, pat: Pat) -> Vec<(Option<Ident>, Ty)> {
        // Assert type for this pattern already synthesized, as we don't synth type of
        // pattern in this function
        assert!(self.tyctx().node_type(pat).is_some());

        match self.hir.pat(pat).kind() {
            hir::pat::PatKind::Unit => vec![(None, Ty::unit())],
            &hir::pat::PatKind::Ident(name, name_id) => {
                vec![(Some(name), self.tyctx().node_type(name_id).unwrap())]
            },
            hir::pat::PatKind::Struct(ty_path, fields, _rest) => {
                let mut tys = fields
                    .iter()
                    .map(|field| (field.name, self.tyctx().tyof(field.pat)))
                    .collect::<Vec<_>>();
                tys.push((
                    Some(self.hir.ty_path(*ty_path).target_name()),
                    self.tyctx().tyof(pat),
                ));
                tys
            },
            &hir::pat::PatKind::Or(lpat, rpat) => self
                .get_pat_inner_tys(lpat)
                .into_iter()
                .chain(self.get_pat_inner_tys(rpat).into_iter())
                .collect(),
        }
    }

    fn synth_value_body(&mut self, expr: Expr) -> TyResult<Ty> {
        self.synth_expr(expr)
    }

    fn synth_body(&mut self, owner_def_id: DefId, body_id: BodyId) -> TyResult<Ty> {
        let Body { params, value } = self.hir.body(body_id);
        // FIXME: Rewrite when `match` added - WHY?

        if params.is_empty() {
            return self.synth_value_body(*value);
        }

        // Set virgin parameters types
        params.iter().try_for_each(|&pat| {
            self.synth_pat(pat)?;
            Ok(())
        })?;

        let body_ex = self.add_fresh_kind_ex_as_ty();

        tcdbg!(self, add body_ex.0, "function body");

        self.under_ctx(|this| {
            let body_ty = this.check_discard_err(self.hir.body(body_id).value, body_ex.1);

            // If we know of which type function parameter is -- check inferred one against
            // it
            // FIXME: Kinda useless
            // let param_ty = if let Some(early) = early_param_ty {
            //     this.check_ty_discard_err(Spanned::new(this.hir.pat(param).span(),
            // param_ty), early) } else {
            //     param_ty
            // };

            // Parameter types after body check with applied context
            // let params_tys = params
            //     .iter()
            //     .copied()
            //     .map(|pat| self.tyctx().node_type(pat).unwrap().apply_ctx(this))
            //     .collect::<Vec<_>>();

            // let generalized_params_tys =
            // params_tys.iter().copied().zip(params.iter()).fold((), |ty| {
            //     if let Some(ex) = ty.as_ex() {
            //         let var = Ty::next_ty_var_id(name)
            //         self.solve(ex, Ty::var(var).mono());
            //     }
            // });

            let body_ty = body_ty.apply_ctx(this);

            let params_tys = params
                .iter()
                .copied()
                .map(|pat| this.tyctx().node_type(pat).unwrap())
                .collect::<Vec<_>>();

            // For each unsolved existential we produce new level of universal
            // quantification. So, e.g. `id a = a` will give us `forall a. a ->
            // a` type FIXME: Clone
            let generalized_func_ty = params.iter().copied().fold(
                Ty::func(Some(owner_def_id), params_tys, body_ty),
                |func_ty, param| {
                    let param_tys = this.get_pat_inner_tys(param);
                    param_tys
                        .iter()
                        .copied()
                        .fold(func_ty, |func_ty, (name, param_inner_ty)| {
                            if let Some(ex) = param_inner_ty.as_ex() {
                                let var = Ty::next_ty_var_id(name);
                                this.solve(ex, Ty::var(var).mono());
                                Ty::forall(var, func_ty)
                            } else if let Some(kind_ex) = param_inner_ty.as_kind_ex() {
                                let kind_var = Kind::next_kind_var_id(name);
                                this.solve_kind_ex(kind_ex, Kind::new_var(kind_var).mono());
                                Ty::ty_kind(Kind::new_forall(kind_var, Kind::new_ty(func_ty)))
                            } else {
                                func_ty
                            }
                        })
                },
            );

            Ok(generalized_func_ty)
        })
    }

    fn synth_call(&mut self, call: &Call, _expr_id: Expr) -> TyResult<Ty> {
        let lhs_ty = self.synth_expr(call.lhs)?;
        let lhs_ty = lhs_ty.apply_ctx(self);
        self._synth_call(
            Spanned::new(self.hir.expr(call.lhs).span(), Typed::new(call.lhs, lhs_ty)),
            &call.args,
        )
    }

    fn _synth_call(&mut self, lhs: Spanned<Typed<Expr>>, args: &[Expr]) -> TyResult<Ty> {
        verbose!("Synthesize call {lhs} with args {:?}", args);

        let span = lhs.span();
        let lhs_expr = *lhs.node().node();
        let lhs_ty = lhs.node().ty();

        match lhs_ty.kind() {
            TyKind::Error => Ok(lhs_ty),
            &TyKind::Existential(ex) => {
                // // FIXME: Under context or `try_to` to escape types?
                self.try_to(|this| {
                    let params_exes = this.add_fresh_common_ex_list(args.len());

                    tcdbg!(this, add_list params_exes.iter().map(|(ex, _)| ex), format!("parameters of {} as callable", ex.colorized()));

                    let body_ex = this.add_fresh_common_ex();

                    tcdbg!(this, add body_ex.0, format!("body of {} as callable", ex.colorized()));

                    let func_ty = Ty::func(
                        None,
                        params_exes.iter().map(|&(_, ty)| ty).collect(),
                        body_ex.1,
                    );
                    this.solve(ex, func_ty.mono());

                    // TODO: Can we infer param type from application as below in Func? - Yes, read
                    //  "Let arguments go first", but I don't like "from the outside in" inferring.
                    args.iter()
                        .copied()
                        .zip(params_exes.iter().copied())
                        .try_for_each(|(arg, (_, param_ex_ty))| {
                            this.check(arg, param_ex_ty)?;
                            Ok(())
                        })?;

                    Ok(body_ex.1)
                })
            },
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                args.iter()
                    .copied()
                    .zip(params.iter().copied())
                    .try_for_each(|(arg, param)| {
                        self.check(arg, param)?;
                        Ok(())
                    })?;

                Ok(*body)
            },
            &TyKind::Forall(alpha, ty) => {
                let alpha_ex = self.add_fresh_common_ex();
                tcdbg!(self, add alpha_ex.0, format!("call forall {}. {}", alpha_ex.0.colorized(), ty.with_sess(self.sess())));

                let substituted_ty = ty.substitute(alpha, alpha_ex.1);
                let body_ty = self._synth_call(
                    Spanned::new(span, Typed::new(lhs_expr, substituted_ty)),
                    args,
                );

                let func_def_id = self.deep_func_def_id(ty);

                self.tyctx_mut()
                    .bind_ty_var(func_def_id, lhs_expr, alpha, alpha_ex.1);

                body_ty
            },
            &TyKind::Kind(kind) => match kind.sort() {
                &KindSort::Ty(ty) => {
                    self._synth_call(Spanned::new(span, Typed::new(lhs_expr, ty)), args)
                },
                &KindSort::Ex(ex) => {
                    self.try_to(|this| {
                        let params_exes = this.add_fresh_common_ex_list(args.len());

                        let body_ex = this.add_fresh_common_ex();
                        let func_ty: Kind = Ty::func(
                            None,
                            params_exes.iter().map(|&(_, ty)| ty).collect(),
                            body_ex.1,
                        )
                        .into();
                        this.solve_kind_ex(ex, func_ty.mono());

                        // TODO: Can we infer param type from application as below in Func? - Yes,
                        // read  "Let arguments go first", but I don't like
                        // "from the outside in" inferring.
                        args.iter()
                            .copied()
                            .zip(params_exes.iter().copied())
                            .try_for_each(|(arg, (_, param_ex_ty))| {
                                this.check(arg, param_ex_ty)?;
                                Ok(())
                            })?;

                        Ok(body_ex.1)
                    })
                },
                &KindSort::Forall(alpha, kind) => {
                    let alpha_ex = self.add_fresh_kind_ex();
                    let subst_kind = kind.substitute(alpha, alpha_ex.1);
                    let body_ty = self._synth_call(
                        Spanned::new(span, Typed::new(lhs_expr, subst_kind.into())),
                        args,
                    );

                    body_ty
                },
                KindSort::Abs(..) | KindSort::Var(_) => {
                    MessageBuilder::error()
                        .text(format!(
                            "{} cannot be called",
                            lhs_ty.with_sess(self.sess())
                        ))
                        .span(span)
                        .label(
                            span,
                            format!(
                                "has type {} which cannot be called",
                                lhs_ty.with_sess(self.sess())
                            ),
                        )
                        .emit(self);

                    Err(TypeckErr::Reported)
                },
            },
            // FIXME: Type variable can be a callee? - "I'm pretty sure, that all tyvars here must
            // already be replaced with existentials"
            _ => {
                MessageBuilder::error()
                    .text(format!(
                        "{} cannot be called",
                        lhs_ty.with_sess(self.sess())
                    ))
                    .span(span)
                    .label(
                        span,
                        format!(
                            "has type {} which cannot be called",
                            lhs_ty.with_sess(self.sess())
                        ),
                    )
                    .emit(self);

                Err(TypeckErr::Reported)
            },
        }
    }
}
