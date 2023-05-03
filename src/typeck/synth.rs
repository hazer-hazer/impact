use super::{
    ty::{ExKind, FloatKind, IntKind, Ty, TyKind, VariantId},
    TyResult, TypeckErr, Typecker, Typed,
};
use crate::{
    cli::verbose,
    hir::{
        self,
        expr::{Call, ExprKind, Lit, TyExpr},
        item::{ItemId, ItemKind, Mod},
        stmt::{Local, StmtKind},
        Block, Body, BodyId, Expr, ExprDefKind, ExprPath, ExprRes, Pat, Stmt,
    },
    message::message::MessageBuilder,
    resolve::def::{DefId, DefKind},
    span::{sym::Ident, Spanned, WithSpan},
    typeck::kind::{Kind, KindSort},
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
        };

        let ty = ty.apply_ctx(self.ctx());
        self.type_inferring_node(hir_id, ty);

        TyResult::Ok(Ty::unit())
    }

    fn synth_stmt(&mut self, stmt: Stmt) -> TyResult<Ty> {
        let stmt = self.hir.stmt(stmt);

        match stmt.kind() {
            &StmtKind::Expr(expr) => {
                self.synth_expr(expr)?;
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
        let local_ty = self.synth_expr(local.value)?.apply_ctx(self.ctx());
        self.type_inferring_node(local.id, local_ty);
        Ok(Ty::unit())
    }

    pub fn synth_expr(&mut self, expr_id: Expr) -> TyResult<Ty> {
        let expr_ty = match self.hir.expr(expr_id).kind() {
            ExprKind::Lit(lit) => self.synth_lit(&lit),
            &ExprKind::Path(path) => self.synth_path(path),
            &ExprKind::Block(block) => self.synth_block(block),
            ExprKind::Lambda(lambda) => self.synth_body(lambda.def_id, lambda.body_id),
            ExprKind::Call(call) => self.synth_call(&call, expr_id),
            &ExprKind::Let(block) => self.under_new_ctx(|this| this.synth_block(block)),
            ExprKind::Ty(ty_expr) => self.synth_ty_expr(&ty_expr),
            // &ExprKind::FieldAccess(lhs, field) => self.synth_field_access_expr(lhs, field,
            // expr_id),
            &ExprKind::Builtin(bt) => Ok(self.tyctx().builtin(bt.into())),
        }?;

        verbose!("Synthesized type of expression {expr_id} = {expr_ty}");

        let expr_ty = expr_ty.apply_ctx(self.ctx());

        self.type_inferring_node(expr_id, expr_ty);

        Ok(expr_ty)
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
                ExprDefKind::External
                | ExprDefKind::FieldAccessor
                | ExprDefKind::Ctor
                | ExprDefKind::Func
                | ExprDefKind::Value => Ok(self.tyctx().def_ty(def_id).expect(&format!(
                    "Expected type of def {} ty be collected at conv stage",
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

    fn synth_field_access_expr(
        &mut self,
        lhs: Expr,
        field_name: Ident,
        expr_id: Expr,
    ) -> TyResult<Ty> {
        let lhs_ty = self.synth_expr(lhs)?;

        // FIXME: Really bug if not adt?
        let adt = lhs_ty.as_adt().unwrap();
        let variants = &adt.variants;

        assert!(!variants.is_empty());

        if variants.len() > 1 {
            // TODO: Better message
            MessageBuilder::error()
                .span(field_name.span())
                .text("Cannot get field from enum".to_string())
                .emit_single_label(self);
        }

        let variant_id = VariantId::new(0);
        let variant = &variants[variant_id];
        let field = variant
            .fields
            .iter_enumerated()
            .find(|(_, field)| field.name == field_name);

        if let Some(field) = field {
            self.tyctx_mut().set_field_index(expr_id, field.0);
            Ok(field.1.ty)
        } else {
            MessageBuilder::error()
                .span(self.hir.expr(expr_id).span())
                .text(format!(
                    "Data type {} does not have field {}",
                    self.tyctx().ty_name(lhs_ty).unwrap(),
                    field_name
                ))
                .emit_single_label(self);
            Err(TypeckErr::Reported)
        }
    }

    fn synth_block(&mut self, block: Block) -> TyResult<Ty> {
        let block = self.hir.block(block);

        self.under_new_ctx(|this| {
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

    // TODO: If needs to be used -- move to HIR methods
    // fn get_pat_names(&self, pat: Pat) -> Vec<Ident> {
    //     match self.hir.pat(pat).kind() {
    //         hir::pat::PatKind::Unit => vec![],
    //         &hir::pat::PatKind::Ident(name) => vec![name],
    //     }
    // }

    /// Get pattern type based on pattern, e.g. unit pattern `()` definitely is
    /// of unit type.
    // fn get_early_pat_type(&self, pat: Pat) -> Option<Ty> {
    //     match self.hir.pat(pat).kind() {
    //         hir::pat::PatKind::Unit => Some(Ty::unit()),
    //         hir::pat::PatKind::Ident(_) => None,
    //     }
    // }

    // TODO: Update if type annotations added
    fn get_param_type(&mut self, pat: Pat) -> Vec<(Pat, Ty)> {
        match self.hir.pat(pat).kind() {
            hir::pat::PatKind::Unit => vec![(pat, Ty::unit())],
            &hir::pat::PatKind::Ident(_) => {
                vec![(pat, self.add_fresh_kind_ex_as_ty().1)]
            },
        }
    }

    fn get_typed_pat(&mut self, pat: Pat) -> Ty {
        match self.hir.pat(pat).kind() {
            hir::pat::PatKind::Unit => Ty::unit(),

            // Assumed that all names in pattern are typed, at least as existentials
            &hir::pat::PatKind::Ident(_) => self
                .tyctx_mut()
                .node_type(pat)
                .unwrap()
                .apply_ctx(self.ctx()),
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

        let params_pats_tys = params
            .iter()
            .copied()
            .map(|param| self.get_param_type(param))
            .collect::<Vec<_>>();

        let body_ex = self.add_fresh_kind_ex_as_ty();

        self.under_new_ctx(|this| {
            params_pats_tys.iter().flatten().for_each(|&(pat, ty)| {
                this.type_inferring_node(pat, ty);
            });

            let body_ty = this.check_discard_err(self.hir.body_value(body_id), body_ex.1);
            verbose!(
                "Body ty {body_ty} checked against body existential {}",
                body_ex.1
            );

            // Apply context to function parameter to get its inferred type.
            let params_tys = params
                .iter()
                .copied()
                .map(|param| this.get_typed_pat(param))
                .collect::<Vec<_>>();

            // If we know of which type function parameter is -- check inferred one against
            // it
            // FIXME: Kinda useless
            // let param_ty = if let Some(early) = early_param_ty {
            //     this.check_ty_discard_err(Spanned::new(this.hir.pat(param).span(),
            // param_ty), early) } else {
            //     param_ty
            // };

            params
                .iter()
                .copied()
                .zip(params_tys.iter().copied())
                .for_each(|(param, ty)| this.type_inferring_node(param, ty));

            // FIXME: Clone
            let func_ty = params_tys.iter().fold(
                Ty::func(Some(owner_def_id), params_tys.clone(), body_ty),
                |func_ty, &param_ty| {
                    if let Some(ex) = this.is_unsolved_ex(param_ty) {
                        let ty_var = Ty::next_ty_var_id(None);
                        this.solve(ex, Ty::var(ty_var).mono());
                        Ty::forall(ty_var, func_ty)
                    } else if let Some(ex) = this.is_unsolved_kind_ex(param_ty) {
                        let kind_var = Kind::next_kind_var_id(None);
                        this.solve_kind_ex(ex, Kind::new_var(kind_var).as_mono().unwrap());
                        Ty::ty_kind(Kind::new_forall(kind_var, Kind::new_ty(func_ty)))
                    } else {
                        func_ty
                    }
                },
            );

            verbose!("Func ty {func_ty}");

            Ok(func_ty)
        })
    }

    // fn synth_lambda_ex(&mut self, lambda: &Lambda) -> TyResult<Ty> {
    //     // FIXME: Rewrite when `match` added

    //     let param_names = self.get_pat_names(lambda.param);

    //     let param_exes = param_names.iter().fold(HashMap::new(), |mut exes,
    // &name| {         // FIXME: Should sub-pattern names existentials be
    // defined outside this context?         let ex =
    // self.add_fresh_common_ex();         self.type_term(name, ex.1);

    //         assert!(exes.insert(name, ex).is_none());
    //         exes
    //     });

    //     let body_ex = self.add_fresh_common_ex();

    //     self.under_new_ctx(|this| {
    //         // FIXME: Optimize fold moves of vec
    //         param_exes.iter().for_each(|(&name, &ex)| {
    //             // FIXME: Should sub-pattern names existentials be defined
    // outside this context?             this.type_term(name, ex.1);
    //         });

    //         let body_ty = this.check(self.hir.body_value(lambda.body),
    // body_ex.1)?;

    //         let param_ty = this.get_typed_pat(lambda.param);

    //         this.type_inferring_node(lambda.param, param_ty);

    //         Ok(Ty::func(param_ty, body_ty))
    //     })
    // }

    fn synth_call(&mut self, call: &Call, _expr_id: Expr) -> TyResult<Ty> {
        let lhs_ty = self.synth_expr(call.lhs)?;
        let lhs_ty = lhs_ty.apply_ctx(self.ctx());
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

                    let body_ex = this.add_fresh_common_ex();
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
                        .text(format!("{} cannot be called", lhs_ty))
                        .span(span)
                        .label(span, format!("has type {} which cannot be called", lhs_ty))
                        .emit(self);

                    Err(TypeckErr::Reported)
                },
            },
            // FIXME: Type variable can be a callee? - "I'm pretty sure, that all tyvars here must
            // already be replaced with existentials"
            _ => {
                MessageBuilder::error()
                    .text(format!("{} cannot be called", lhs_ty))
                    .span(span)
                    .label(span, format!("has type {} which cannot be called", lhs_ty))
                    .emit(self);

                Err(TypeckErr::Reported)
            },
        }
    }
}
