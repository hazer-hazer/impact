use crate::{
    cli::verbose,
    hir::{
        self,
        expr::{Block, Call, Expr, ExprKind, Lambda, Lit, TyExpr},
        item::{ItemId, ItemKind, Mod},
        stmt::{Stmt, StmtKind},
        HirId, Path,
    },
    message::message::MessageBuilder,
    resolve::def::DefKind,
    span::span::WithSpan,
    typeck::ty::Subst,
};

use super::ty::{PrimTy, Ty, TyError, TyKind, TyResult};

use super::Typecker;

impl<'hir> Typecker<'hir> {
    pub fn synth_item(&mut self, item: ItemId) -> TyResult<Ty> {
        match self.sess.def_table.get_def(item.def_id()).unwrap().kind() {
            &DefKind::Builtin(bt) => return Ok(self.tyctx().builtin_ty(bt)),
            DefKind::DeclareBuiltin => return Ok(self.tyctx_mut().unit()),
            _ => {},
        }

        let item = self.hir.item(item);

        let ty = match item.kind() {
            ItemKind::TyAlias(_ty) => {
                // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                self.conv_ty_alias(item.def_id());
                self.tyctx_mut().unit()
            },
            ItemKind::Mod(Mod { items }) => {
                for item in items {
                    self.synth_item(*item)?;
                }
                self.tyctx_mut().unit()
            },
            ItemKind::Decl(decl) => {
                let value_ty = self.synth_expr(decl.value)?;
                self.type_term(item.name(), value_ty);

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

        Ok(self.tyctx_mut().unit())
    }

    pub fn synth_expr(&mut self, expr_id: Expr) -> TyResult<Ty> {
        verbose!("Synth type of expression {}", expr_id);

        let expr = self.hir.expr(expr_id);
        let expr_ty = match expr.kind() {
            ExprKind::Lit(lit) => self.synth_lit(lit),
            ExprKind::Path(path) => self.synth_path(path.0),
            &ExprKind::Block(block) => self.synth_block(block),
            ExprKind::Lambda(lambda) => self.synth_lambda(lambda),
            ExprKind::Call(call) => self.synth_call(call),
            &ExprKind::Let(block) => self.under_new_ctx(|this| this.synth_block(block)),
            ExprKind::Ty(ty_expr) => self.synth_ty_expr(ty_expr),
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

        Ok(self.tyctx_mut().prim(prim))
    }

    fn synth_path(&mut self, path: Path) -> TyResult<Ty> {
        let path = self.hir.path(path);
        self.lookup_typed_term_ty(path.target_name())
            .ok_or_else(|| {
                MessageBuilder::error()
                    .span(path.span())
                    .text(format!("Term {} does not have a type", path))
                    .emit_single_label(self);
                TyError()
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
                .map_or(Ok(this.tyctx_mut().unit()), |&expr| this.synth_expr(expr));

            this.default_number_exes();

            res_ty
        })
    }

    fn synth_lambda(&mut self, lambda: &Lambda) -> TyResult<Ty> {
        let param_name = match self.hir.pat(lambda.param).kind() {
            &hir::pat::PatKind::Ident(name) => name,
        };
        let param_name_ex = self.add_fresh_common_ex();

        let body_ex = self.add_fresh_common_ex();

        self.under_new_ctx(|this| {
            this.type_term(param_name, param_name_ex.1);

            let body_ty = this.check(lambda.body, body_ex.1)?;

            let param_ty = this.apply_ctx_on(param_name_ex.1);

            this.tyctx_mut().type_node(lambda.param, param_ty);

            Ok(this.tyctx_mut().func(param_ty, body_ty))
        })
    }

    fn synth_call(&mut self, call: &Call) -> TyResult<Ty> {
        let lhs_ty = self.synth_expr(call.lhs)?;
        let lhs_ty = self.apply_ctx_on(lhs_ty);
        self._synth_call(lhs_ty, call.arg)
    }

    fn _synth_call(&mut self, lhs_ty: Ty, arg: Expr) -> TyResult<Ty> {
        verbose!(
            "Synthesize call {} with arg {}",
            self.tyctx().pp(lhs_ty),
            arg
        );

        match self.tyctx().ty(lhs_ty).kind() {
            // FIXME: Or return Ok(lhs_ty)?
            TyKind::Error => Err(TyError()),
            TyKind::Unit | TyKind::Prim(_) | TyKind::Var(_) => todo!("Non-callable type"),
            &TyKind::Existential(ex) => {
                // // FIXME: Under context or `try_to` to escape types?
                self.try_to(|this| {
                    let param_ex = this.add_fresh_common_ex();
                    let body_ex = this.add_fresh_common_ex();
                    let func_ty = this.tyctx_mut().func(param_ex.1, body_ex.1);
                    this.solve(ex, func_ty);

                    this.check(arg, param_ex.1)?;
                    Ok(body_ex.1)
                })
            },
            &TyKind::Func(param, body) => {
                self.check(arg, param)?;
                Ok(body)
            },
            &TyKind::Forall(alpha, ty) => {
                let alpha_ex = self.add_fresh_common_ex();
                let substituted_ty = self.substitute(ty, Subst::Name(alpha), alpha_ex.1);
                self._synth_call(substituted_ty, arg)
            },
        }
    }
}
