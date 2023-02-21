use std::collections::HashMap;

use crate::{
    cli::verbose,
    hir::{
        self,
        expr::{Block, Call, Expr, ExprKind, Lambda, Lit, TyExpr},
        item::{Decl, ItemId, ItemKind, Mod},
        pat::Pat,
        stmt::{Stmt, StmtKind},
        HirId, Path,
    },
    message::message::MessageBuilder,
    resolve::def::DefKind,
    span::span::{Ident, Spanned, WithSpan},
    typeck::ty::Subst,
};

use super::{
    ty::{PrimTy, Ty, TyKind},
    TyResult, TypeckErr,
};

use super::Typecker;
impl<'hir> Typecker<'hir> {
    pub fn synth_item(&mut self, item: ItemId) -> TyResult<Ty> {
        match self.sess.def_table.get_def(item.def_id()).unwrap().kind() {
            &DefKind::Builtin(bt) => return Ok(self.tyctx().builtin_ty(bt)),
            DefKind::DeclareBuiltin => return Ok(Ty::unit()),
            _ => {},
        }

        let ty = match self.hir.item(item).kind() {
            ItemKind::TyAlias(_ty) => {
                verbose!("Synth ty alias {}", item.def_id());
                // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                self.conv_ty_alias(item.def_id());
                Ty::unit()
            },
            ItemKind::Mod(Mod { items }) => {
                // FIXME: How not to clone?
                for item in items.clone() {
                    self.synth_item(item)?;
                }
                Ty::unit()
            },
            &ItemKind::Decl(Decl { value }) => {
                verbose!("Synth declaration {}", item.def_id());

                let value_ty = self.synth_expr(value)?;

                self.type_term(self.hir.item(item).name(), value_ty);

                // Note: Actually, declaration type is a unit type, but we save it
                // TODO: Add encapsulation layer such as `get_def_ty` (with closed access to TyCtx::typed) which will check if definition CAN have a type
                value_ty
            },
        };

        self.tyctx_mut()
            .type_node(HirId::new_owner(item.def_id()), ty);

        TyResult::Ok(ty)
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
        }

        Ok(Ty::unit())
    }

    pub fn synth_expr(&mut self, expr_id: Expr) -> TyResult<Ty> {
        verbose!("Synth type of expression {}", expr_id);

        let expr_ty = match self.hir.expr(expr_id).kind() {
            ExprKind::Lit(lit) => self.synth_lit(&lit),
            ExprKind::Path(path) => self.synth_path(path.0),
            &ExprKind::Block(block) => self.synth_block(block),
            ExprKind::Lambda(lambda) => self.synth_lambda_generic(&lambda),
            ExprKind::Call(call) => self.synth_call(&call, expr_id),
            &ExprKind::Let(block) => self.under_new_ctx(|this| this.synth_block(block)),
            ExprKind::Ty(ty_expr) => self.synth_ty_expr(&ty_expr),
        }?;

        let expr_ty = self.apply_ctx_on(expr_ty);

        self.tyctx_mut().type_node(expr_id, expr_ty);

        Ok(expr_ty)
    }

    fn synth_lit(&mut self, lit: &Lit) -> TyResult<Ty> {
        let prim = match lit {
            Lit::Bool(_) => PrimTy::Bool,
            Lit::String(_) => PrimTy::String,

            &Lit::Int(_, kind) => return Ok(self.conv_int_kind(kind)),
            &Lit::Float(_, kind) => return Ok(self.conv_float_kind(kind)),
        };

        Ok(Ty::prim(prim))
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

    fn get_pat_names(&self, pat: Pat) -> Vec<Ident> {
        match self.hir.pat(pat).kind() {
            hir::pat::PatKind::Unit => vec![],
            &hir::pat::PatKind::Ident(name) => vec![name],
        }
    }

    fn get_early_pat_type(&self, pat: Pat) -> Option<Ty> {
        match self.hir.pat(pat).kind() {
            hir::pat::PatKind::Unit => Some(Ty::unit()),
            hir::pat::PatKind::Ident(_) => None,
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

    fn synth_lambda_generic(&mut self, lambda: &Lambda) -> TyResult<Ty> {
        // FIXME: Rewrite when `match` added

        // Get parameter type from pattern if possible, e.g. `()` is of type `()`.
        let early_param_ty = self.get_early_pat_type(lambda.param);

        let param_names = self.get_pat_names(lambda.param);

        let param_exes = param_names.iter().fold(HashMap::new(), |mut exes, &name| {
            // FIXME: Should sub-pattern names existentials be defined outside this context?

            let ex = self.add_fresh_common_ex();

            //
            // self.type_term(name, ex.1);

            assert!(exes.insert(name, ex).is_none());
            exes
        });

        let body_ex = self.add_fresh_common_ex();

        self.under_new_ctx(|this| {
            // FIXME: Optimize fold moves of vec
            param_exes.iter().for_each(|(&name, &ex)| {
                // FIXME: Should sub-pattern names existentials be defined outside this context?
                this.type_term(name, ex.1);
            });

            let body_ty = this.check_discard_err(lambda.body, body_ex.1);

            // Apply context to function parameter to get its type.
            let param_ty = this.get_typed_pat(lambda.param);

            // If we know of which type function parameter is -- check inferred one against it
            let param_ty = if let Some(early) = early_param_ty {
                this.check_ty_discard_err(
                    Spanned::new(this.hir.pat(lambda.param).span(), param_ty),
                    early,
                )
            } else {
                param_ty
            };

            verbose!("Param ty {}", param_ty);

            this.tyctx_mut().type_node(lambda.param, param_ty);

            let func_ty =
                param_exes
                    .iter()
                    .fold(Ty::func(param_ty, body_ty), |ty, (&name, &(ex, _))| {
                        verbose!(
                            "Func type generation {ty}; push {name}: {ex} = {:?}",
                            this.get_solution(ex)
                        );
                        if let Some(_) = this.get_solution(ex) {
                            ty
                        } else {
                            // FIXME: Check type variable resolution
                            let ty_var = Ty::next_ty_var_id();
                            this.solve(ex, Ty::var(ty_var));
                            Ty::forall(ty_var, ty)
                        }
                    });

            verbose!("Func ty {func_ty}");

            Ok(func_ty)
        })
    }

    fn synth_lambda_ex(&mut self, lambda: &Lambda) -> TyResult<Ty> {
        // FIXME: Rewrite when `match` added

        let param_names = self.get_pat_names(lambda.param);

        let param_exes = param_names.iter().fold(HashMap::new(), |mut exes, &name| {
            // FIXME: Should sub-pattern names existentials be defined outside this context?
            let ex = self.add_fresh_common_ex();
            self.type_term(name, ex.1);

            assert!(exes.insert(name, ex).is_none());
            exes
        });

        let body_ex = self.add_fresh_common_ex();

        self.under_new_ctx(|this| {
            // FIXME: Optimize fold moves of vec
            param_exes.iter().for_each(|(&name, &ex)| {
                // FIXME: Should sub-pattern names existentials be defined outside this context?
                this.type_term(name, ex.1);
            });

            let body_ty = this.check(lambda.body, body_ex.1)?;

            let param_ty = this.get_typed_pat(lambda.param);

            this.tyctx_mut().type_node(lambda.param, param_ty);

            Ok(Ty::func(param_ty, body_ty))
        })
    }

    fn synth_call(&mut self, call: &Call, expr_id: Expr) -> TyResult<Ty> {
        let lhs_ty = self.synth_expr(call.lhs)?;
        let lhs_ty = self.apply_ctx_on(lhs_ty);
        self._synth_call(
            Spanned::new(self.hir.expr(call.lhs).span(), lhs_ty),
            call.arg,
            expr_id,
        )
    }

    fn _synth_call(&mut self, lhs_ty: Spanned<Ty>, arg: Expr, call_expr: Expr) -> TyResult<Ty> {
        verbose!("Synthesize call {} with arg {}", lhs_ty, arg);

        let span = lhs_ty.span();
        let lhs_ty = lhs_ty.node();

        match lhs_ty.kind() {
            // FIXME: Or return Ok(lhs_ty)?
            TyKind::Error => Ok(*lhs_ty),
            TyKind::Unit | TyKind::Prim(_) | TyKind::Var(_) => {
                MessageBuilder::error()
                    .text(format!("{} cannot be called", lhs_ty))
                    .span(span)
                    .label(span, format!("has type {} which cannot be called", lhs_ty))
                    .emit(self);

                Err(TypeckErr::Reported)
            },
            &TyKind::Existential(ex) => {
                // // FIXME: Under context or `try_to` to escape types?
                self.try_to(|this| {
                    let param_ex = this.add_fresh_common_ex();

                    let body_ex = this.add_fresh_common_ex();
                    let func_ty = Ty::func(param_ex.1, body_ex.1);
                    this.solve(ex, func_ty);

                    // TODO: Can we infer param type from application as below in Func?
                    this.check(arg, param_ex.1)?;

                    Ok(body_ex.1)
                })
            },
            &TyKind::Func(param, body) => {
                // TODO: Let arguments go first
                // if let Some(param_ex) = param.as_ex() {
                //     todo!();
                //     let arg = self.synth_expr(arg)?;
                //     self.add_func_param_sol(param_ex, arg);
                // } else {
                //     self.check_discard_err(arg, param);
                // }
                // self.check(arg, param)?;

                self.check_discard_err(arg, param);

                Ok(body)
            },
            &TyKind::Forall(alpha, ty) => {
                let alpha_ex = self.add_fresh_common_ex();
                let substituted_ty = self.substitute(ty, Subst::Var(alpha), alpha_ex.1);
                let body_ty = self._synth_call(Spanned::new(span, substituted_ty), arg, call_expr);

                self.tyctx_mut().bind_ty_var(call_expr, alpha, alpha_ex.1);

                body_ty
            },
        }
    }
}
