use crate::{
    cli::verbose,
    hir::{
        self,
        expr::{Block, Call, Expr, ExprKind, Lit, TyExpr},
        item::{ItemId, ItemKind, Mod},
        pat::Pat,
        stmt::{Local, Stmt, StmtKind},
        Body, BodyId, HirId, Path,
    },
    message::message::MessageBuilder,
    resolve::def::{DefId, DefKind},
    span::span::{Ident, Spanned, WithSpan},
    typeck::ty::Subst,
};

use super::{
    ty::{Ty, TySort},
    TyResult, TypeckErr, Typed,
};

use super::Typecker;
impl<'hir> Typecker<'hir> {
    pub fn synth_item(&mut self, item: ItemId) -> TyResult<Ty> {
        let hir_id = HirId::new_owner(item.def_id());
        match self.sess.def_table.get_def(item.def_id()).kind() {
            DefKind::DeclareBuiltin => {
                self.tyctx_mut().type_node(
                    hir_id,
                    Ty::func(
                        Some(item.def_id()),
                        vec![Ty::str()],
                        Ty::var(Ty::next_ty_var_id()),
                    ),
                );
                return Ok(Ty::unit());
            },
            _ => {},
        }

        let item = self.hir.item(item);

        let ty = match item.kind() {
            ItemKind::TyAlias(_ty) => {
                verbose!("Synth ty alias {}", item.def_id());
                // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                return Ok(self.conv_ty_alias(item.def_id()));
            },
            ItemKind::Mod(Mod { items }) => {
                // FIXME: How not to clone?
                for item in items.clone() {
                    self.synth_item(item)?;
                }
                Ty::unit()
            },
            // Note: Actually, declaration type is a unit type, but we save it
            // TODO: Add encapsulation layer such as `get_def_ty` (with closed access to TyCtx::typed) which will check if definition CAN have a type
            // TODO: Merge these branches?
            &ItemKind::Value(value) => {
                let value_ty = self.synth_body(item.def_id(), value)?;
                self.type_term(item.name(), value_ty);
                value_ty
            },
            &ItemKind::Func(body) => {
                let value_ty = self.synth_body(item.def_id(), body)?;
                self.type_term(item.name(), value_ty);
                value_ty
            },
            ItemKind::ExternItem(extern_item) => {
                let ty = self.conv(extern_item.ty);
                self.type_term(item.name(), ty);
                ty
            },
            // ItemKind::ExternBlock(items) => {
            //     items.iter().for_each(|item| {
            //         let ty = self.conv(item.ty);
            //         self.type_term(item.name, ty);
            //         self.tyctx_mut().type_node(item.id, ty)
            //     });
            //     Ty::unit()
            // },
        };

        self.tyctx_mut().type_node(hir_id, ty);

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
        let local_ty = self.synth_expr(local.value)?;
        self.type_term(local.name, local_ty);
        self.tyctx_mut().type_node(local.def_id.into(), local_ty);
        Ok(Ty::unit())
    }

    pub fn synth_expr(&mut self, expr_id: Expr) -> TyResult<Ty> {
        verbose!("Synth type of expression {}", expr_id);

        let expr_ty = match self.hir.expr(expr_id).kind() {
            ExprKind::Lit(lit) => self.synth_lit(&lit),
            ExprKind::Path(path) => self.synth_path(path.0),
            &ExprKind::Block(block) => self.synth_block(block),
            ExprKind::Lambda(lambda) => self.synth_body(lambda.def_id, lambda.body_id),
            ExprKind::Call(call) => self.synth_call(&call, expr_id),
            &ExprKind::Let(block) => self.under_new_ctx(|this| this.synth_block(block)),
            ExprKind::Ty(ty_expr) => self.synth_ty_expr(&ty_expr),
            &ExprKind::BuiltinExpr(bt) => {
                assert!(bt.is_value());
                Ok(self.tyctx().builtin(bt))
            },
        }?;

        let expr_ty = self.apply_ctx_on(expr_ty);

        self.tyctx_mut().type_node(expr_id, expr_ty);

        Ok(expr_ty)
    }

    fn synth_lit(&mut self, lit: &Lit) -> TyResult<Ty> {
        Ok(match lit {
            Lit::Bool(_) => Ty::bool(),
            Lit::String(_) => Ty::str(),

            &Lit::Int(_, kind) => self.conv_int(kind),
            &Lit::Float(_, kind) => self.conv_float(kind),
        })
    }

    fn synth_path(&mut self, path: Path) -> TyResult<Ty> {
        self.lookup_typed_term_ty(self.hir.path(path).target_name())
            .ok_or_else(|| {
                MessageBuilder::error()
                    .span(self.hir.path(path).span())
                    .text(format!("Term {} does not have a type", path))
                    .emit_single_label(self);
                TypeckErr::Reported
            })
    }

    fn synth_ty_expr(&mut self, ty_expr: &TyExpr) -> TyResult<Ty> {
        // FIXME: Check wf?
        // FIXME: Do we need `try_to`?
        self.try_to(|this| {
            let anno_ty = this.conv(ty_expr.ty);
            this.check(ty_expr.expr, anno_ty)
        })
    }

    fn synth_block(&mut self, block: Block) -> TyResult<Ty> {
        let block = self.hir.block(block);

        self.under_new_ctx(|this| {
            block.stmts().iter().try_for_each(|&stmt| {
                this.synth_stmt(stmt)?;
                Ok(())
            })?;

            let res_ty = block
                .expr()
                .map_or(Ok(Ty::unit()), |&expr| this.synth_expr(expr));

            this.default_number_exes();

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

    /// Get pattern type based on pattern, e.g. unit pattern `()` definitely is of unit type.
    // fn get_early_pat_type(&self, pat: Pat) -> Option<Ty> {
    //     match self.hir.pat(pat).kind() {
    //         hir::pat::PatKind::Unit => Some(Ty::unit()),
    //         hir::pat::PatKind::Ident(_) => None,
    //     }
    // }

    // TODO: Update if type annotations added
    fn get_param_type(&mut self, pat: Pat) -> Vec<(Option<Ident>, Ty)> {
        match self.hir.pat(pat).kind() {
            hir::pat::PatKind::Unit => vec![(None, Ty::unit())],
            &hir::pat::PatKind::Ident(name) => vec![(Some(name), self.add_fresh_common_ex().1)],
        }
    }

    /// Get pattern type based on current context, applying context to
    ///  typed terms that appear in pattern as identifiers (Ident pattern)
    fn get_typed_pat(&self, pat: Pat) -> Ty {
        match self.hir.pat(pat).kind() {
            hir::pat::PatKind::Unit => Ty::unit(),

            // Assumed that all names in pattern are typed, at least as existentials
            &hir::pat::PatKind::Ident(name) => {
                self.apply_ctx_on(self.lookup_typed_term_ty(name).unwrap())
            },
        }
    }

    fn synth_value_body(&mut self, expr: Expr) -> TyResult<Ty> {
        self.synth_expr(expr)
    }

    fn synth_body(&mut self, owner_def_id: DefId, body_id: BodyId) -> TyResult<Ty> {
        let Body { params, value } = self.hir.body(body_id);
        // FIXME: Rewrite when `match` added

        if params.is_empty() {
            return self.synth_value_body(*value);
        }

        let params_names_tys = params
            .iter()
            .copied()
            .map(|param| self.get_param_type(param))
            .collect::<Vec<_>>();

        let body_ex = self.add_fresh_common_ex();

        self.under_new_ctx(|this| {
            params_names_tys.iter().flatten().for_each(|&(name, ty)| {
                if let Some(name) = name {
                    this.type_term(name, ty);
                }
            });

            let body_ty = this.check_discard_err(self.hir.body_value(body_id), body_ex.1);

            // Apply context to function parameter to get its inferred type.
            let params_tys = params
                .iter()
                .copied()
                .map(|param| this.get_typed_pat(param))
                .collect::<Vec<_>>();

            // If we know of which type function parameter is -- check inferred one against it
            // FIXME: Kinda useless
            // let param_ty = if let Some(early) = early_param_ty {
            //     this.check_ty_discard_err(Spanned::new(this.hir.pat(param).span(), param_ty), early)
            // } else {
            //     param_ty
            // };

            params
                .iter()
                .copied()
                .zip(params_tys.iter().copied())
                .for_each(|(param, ty)| this.tyctx_mut().type_node(param, ty));

            // FIXME: Clone
            let func_ty = params_tys.iter().fold(
                Ty::func(Some(owner_def_id), params_tys.clone(), body_ty),
                |func_ty, param_ty| {
                    if let Some(ex) = param_ty.as_ex() {
                        if let None = this.get_solution(ex) {
                            // FIXME: Check type variable resolution
                            let ty_var = Ty::next_ty_var_id();
                            this.solve(ex, Ty::var(ty_var).mono());
                            Ty::forall(ty_var, func_ty)
                        } else {
                            func_ty
                        }
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

    //     let param_exes = param_names.iter().fold(HashMap::new(), |mut exes, &name| {
    //         // FIXME: Should sub-pattern names existentials be defined outside this context?
    //         let ex = self.add_fresh_common_ex();
    //         self.type_term(name, ex.1);

    //         assert!(exes.insert(name, ex).is_none());
    //         exes
    //     });

    //     let body_ex = self.add_fresh_common_ex();

    //     self.under_new_ctx(|this| {
    //         // FIXME: Optimize fold moves of vec
    //         param_exes.iter().for_each(|(&name, &ex)| {
    //             // FIXME: Should sub-pattern names existentials be defined outside this context?
    //             this.type_term(name, ex.1);
    //         });

    //         let body_ty = this.check(self.hir.body_value(lambda.body), body_ex.1)?;

    //         let param_ty = this.get_typed_pat(lambda.param);

    //         this.tyctx_mut().type_node(lambda.param, param_ty);

    //         Ok(Ty::func(param_ty, body_ty))
    //     })
    // }

    fn synth_call(&mut self, call: &Call, _expr_id: Expr) -> TyResult<Ty> {
        let lhs_ty = self.synth_expr(call.lhs)?;
        let lhs_ty = self.apply_ctx_on(lhs_ty);
        self._synth_call(
            Spanned::new(self.hir.expr(call.lhs).span(), Typed::new(call.lhs, lhs_ty)),
            &call.args,
        )
    }

    fn _synth_call(&mut self, lhs: Spanned<Typed<Expr>>, args: &[Expr]) -> TyResult<Ty> {
        verbose!("Synthesize call {} with args {:?}", lhs, args);

        let span = lhs.span();
        let lhs_expr = *lhs.node().node();
        let lhs_ty = lhs.node().ty();

        match lhs_ty.sort() {
            // FIXME: Or return Ok(lhs_ty)?
            TySort::Error => Ok(lhs_ty),
            &TySort::Existential(ex) => {
                // // FIXME: Under context or `try_to` to escape types?
                self.try_to(|this| {
                    // FIXME: Add multiple arguments
                    let params_exes = this.add_fresh_common_ex_list(1);

                    let body_ex = this.add_fresh_common_ex();
                    let func_ty = Ty::func(
                        None,
                        params_exes.iter().map(|&(_, ty)| ty).collect(),
                        body_ex.1,
                    );
                    this.solve(ex, func_ty.mono());

                    // TODO: Can we infer param type from application as below in Func?
                    // FIXME: Add multiple args
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
            TySort::Func(params, body) | TySort::FuncDef(_, params, body) => {
                // TODO?: Let arguments go first
                // if let Some(param_ex) = param.as_ex() {
                //     todo!();
                //     let arg = self.synth_expr(arg)?;
                //     self.add_func_param_sol(param_ex, arg);
                // } else {
                //     self.check_discard_err(arg, param);
                // }
                // self.check(arg, param)?;

                // FIXME: Multiple args

                args.iter()
                    .copied()
                    .zip(params.iter().copied())
                    .try_for_each(|(arg, param)| {
                        self.check(arg, param)?;
                        Ok(())
                    })?;

                Ok(*body)
            },
            &TySort::Forall(alpha, ty) => {
                let alpha_ex = self.add_fresh_common_ex();
                let substituted_ty = ty.substitute(Subst::Var(alpha), alpha_ex.1);
                let body_ty = self._synth_call(
                    Spanned::new(span, Typed::new(lhs_expr, substituted_ty)),
                    args,
                );

                let func_def_id = self.deep_func_def_id(ty);

                self.tyctx_mut()
                    .bind_ty_var(func_def_id, lhs_expr, alpha, alpha_ex.1);

                body_ty
            },
            // FIXME: Type variable can be a callee?
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
