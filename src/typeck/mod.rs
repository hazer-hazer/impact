use crate::{
    hir::{
        self,
        expr::{Block, Call, Expr, ExprKind, Lambda, Lit, TyExpr},
        item::{ItemId, ItemKind, Mod},
        stmt::{Stmt, StmtKind},
        HirMap, Path, HIR,
    },
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{
        def::{DefId, DefKind, DefMap},
        res::ResKind,
    },
    session::{Session, Stage, StageOutput},
    span::span::WithSpan,
    typeck::ty::Subst,
};

use self::{
    ctx::{Ctx, ExistentialId},
    ty::{FloatKind, IntKind, PrimTy, Ty, TyCtx, TyError, TyKind, TyResult},
};

pub mod ctx;
pub mod ty;

struct Typecker<'hir> {
    tyctx: TyCtx,

    hir: &'hir HIR,

    /// Types associated to DefId's (meaning for each item is different!)
    /// - Type alias: `[Type alias DefId] -> [Its converted type]`
    /// - Declaration: `[Declaration DefId] -> [Type of assigned value]`
    def_types: DefMap<Ty>,
    expr_types: HirMap<Ty>,

    msg: MessageStorage,
    sess: Session,
}

impl<'hir> MessageHolder for Typecker<'hir> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'hir> Typecker<'hir> {
    pub fn new(sess: Session, hir: &'hir HIR) -> Self {
        Self {
            tyctx: TyCtx::new(),
            hir,
            sess,
            def_types: Default::default(),
            expr_types: Default::default(),
            msg: Default::default(),
        }
    }

    fn under_ctx<T>(&mut self, ctx: Ctx, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.tyctx.enter_ctx(ctx);
        let res = f(self);
        self.tyctx.exit_ctx();
        res
    }

    fn under_new_ctx<T>(&mut self, f: impl FnMut(&mut Self) -> T) -> T {
        self.under_ctx(Ctx::default(), f)
    }

    fn try_to<T>(&mut self, mut f: impl FnMut(&mut Self) -> TyResult<T>) -> TyResult<T> {
        let restore = self.tyctx.enter_try_mode();
        let res = f(self);
        self.tyctx.exit_try_mode(res, restore)
    }

    fn add_fresh_ex(&mut self) -> (ExistentialId, Ty) {
        let ex = self.tyctx.fresh_ex();
        self.tyctx.add_ex(ex);
        (ex, self.tyctx.existential(ex))
    }

    // Conversion //
    fn conv(&mut self, ty: hir::ty::Ty) -> Ty {
        let ty = self.hir.ty(ty);
        // TODO: Allow recursive?

        match ty.kind {
            hir::ty::TyKind::Unit => self.tyctx.unit(),
            hir::ty::TyKind::Path(path) => self.conv_path(path),
            hir::ty::TyKind::Func(param, body) => {
                let param = self.conv(param);
                let ret = self.conv(body);
                self.tyctx.func(param, ret)
            },
        }
    }

    fn conv_path(&mut self, path: Path) -> Ty {
        let path = self.hir.path(path);
        match path.res().kind() {
            &ResKind::Def(def_id) => {
                let def = self.sess.def_table.get_def(def_id).unwrap();
                match def.kind() {
                    DefKind::TyAlias => {
                        // Path conversion is done linearly, i.e. we get type alias from HIR and convert its type, caching it
                        // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                        self.conv_ty_alias(def_id)
                    },

                    // Non-type definitions from type namespace
                    DefKind::Root | DefKind::Mod => {
                        MessageBuilder::error()
                            .span(path.span())
                            .text(format!("{} item used as type", def.kind()))
                            .emit_single_label(self);

                        self.tyctx.error()
                    },

                    // Definitions from value namespace
                    DefKind::Func | DefKind::Var => unreachable!(),
                }
            },
            _ => unreachable!(),
        }
    }

    fn conv_ty_alias(&mut self, ty_alias_def_id: DefId) -> Ty {
        let def = self.sess.def_table.get_def(ty_alias_def_id).unwrap();
        if let Some(def_ty) = self.def_types.get_flat(ty_alias_def_id) {
            return *def_ty;
        }

        let ty_alias = self.hir.item(ItemId::new(def.def_id.into())).ty_alias();
        let ty = ty_alias.ty;
        let ty = self.conv(ty);
        self.def_types.insert(ty_alias_def_id, ty);
        ty
    }

    fn conv_int_kind(&self, kind: hir::expr::IntKind) -> IntKind {
        match kind {
            hir::expr::IntKind::Unknown => todo!(),
            hir::expr::IntKind::U8 => IntKind::U8,
            hir::expr::IntKind::U16 => IntKind::U16,
            hir::expr::IntKind::U32 => IntKind::U32,
            hir::expr::IntKind::U64 => IntKind::U64,
            hir::expr::IntKind::Uint => IntKind::Uint,
            hir::expr::IntKind::I8 => IntKind::I8,
            hir::expr::IntKind::I16 => IntKind::I16,
            hir::expr::IntKind::I32 => IntKind::I32,
            hir::expr::IntKind::I64 => IntKind::I64,
            hir::expr::IntKind::Int => IntKind::Int,
        }
    }

    fn conv_float_kind(&self, kind: hir::expr::FloatKind) -> FloatKind {
        match kind {
            hir::expr::FloatKind::Unknown => todo!(),
            hir::expr::FloatKind::F32 => FloatKind::F32,
            hir::expr::FloatKind::F64 => FloatKind::F64,
        }
    }

    // Synthesis //
    fn synth_expr(&mut self, expr_id: Expr) -> TyResult<Ty> {
        let expr = self.hir.expr(expr_id);
        let expr_ty = match expr.kind() {
            ExprKind::Unit => Ok(self.tyctx.unit()),
            ExprKind::Lit(lit) => {
                let prim = match lit {
                    Lit::Bool(_) => PrimTy::Bool,
                    Lit::Int(_, kind) => PrimTy::Int(self.conv_int_kind(*kind)),
                    Lit::Float(_, kind) => PrimTy::Float(self.conv_float_kind(*kind)),
                    Lit::String(_) => PrimTy::String,
                };

                Ok(self.tyctx.lit(prim))
            },
            ExprKind::Path(path) => self.synth_path(path.0),
            &ExprKind::Block(block) => self.synth_block(block),
            ExprKind::Infix(_) => todo!(),
            ExprKind::Prefix(_) => todo!(),
            ExprKind::Lambda(lambda) => self.synth_lambda(lambda),
            ExprKind::Call(call) => self.synth_call(call),
            &ExprKind::Let(block) => self.under_new_ctx(|this| this.synth_block(block)),
            ExprKind::Ty(TyExpr { expr, ty: anno }) => {
                let ty = self.conv(*anno);
                self.check(*expr, ty)?;
                Ok(ty)
            },
        }?;

        self.expr_types.insert(expr_id, expr_ty);

        Ok(expr_ty)
    }

    fn synth_path(&self, path: Path) -> TyResult<Ty> {
        let path = self.hir.path(path);
        self.tyctx
            .lookup_typed_term_ty(path.target_name())
            .ok_or(TyError())
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

        block.stmts().iter().try_for_each(|&stmt| {
            self.synth_stmt(stmt)?;
            Ok(())
        })?;

        block
            .expr()
            .map_or(Ok(self.tyctx.unit()), |&expr| self.synth_expr(expr))
    }

    fn synth_lambda(&mut self, lambda: &Lambda) -> TyResult<Ty> {
        // Parameter is a pattern which can contain multiple name bindings
        // FIXME: Should these existentials be inside context?
        let param_names = self.hir.pat_names(lambda.param).map_or(vec![], |names| {
            names
                .iter()
                .map(|&name| (name, self.add_fresh_ex()))
                .collect::<Vec<_>>()
        });

        let body_ex = self.add_fresh_ex();

        self.under_new_ctx(|this| {
            // FIXME: What if pattern w/o name?
            param_names.iter().for_each(|(name, name_ex)| {
                this.tyctx.type_term(*name, name_ex.1);
            });

            this.check(lambda.body, body_ex.1)
        })
    }

    fn synth_call(&mut self, call: &Call) -> TyResult<Ty> {
        let lhs_ty = self.synth_expr(call.lhs)?;
        let lhs_ty = self.tyctx.apply_on(lhs_ty);
        self._synth_call(lhs_ty, call.arg)
    }

    fn _synth_call(&mut self, lhs_ty: Ty, arg: Expr) -> TyResult<Ty> {
        match self.tyctx.ty(lhs_ty).kind() {
            // FIXME: Or return Ok(lhs_ty)?
            TyKind::Error => Err(TyError()),
            TyKind::Unit | TyKind::Lit(_) | TyKind::Var(_) => todo!("Non-callable type"),
            &TyKind::Existential(ex) => {
                // // FIXME: Under context or `try_to` to escape types?
                self.try_to(|this| {
                    let param_ex = this.add_fresh_ex();
                    let body_ex = this.add_fresh_ex();
                    let func_ty = this.tyctx.func(param_ex.1, body_ex.1);
                    this.tyctx.solve(ex, func_ty);

                    this.check(arg, param_ex.1)?;
                    Ok(body_ex.1)
                })
            },
            &TyKind::Func(param, body) => {
                self.check(arg, param)?;
                Ok(body)
            },
            &TyKind::Forall(alpha, ty) => {
                let alpha_ex = self.add_fresh_ex();
                let substituted_ty = self.tyctx.substitute(ty, Subst::Name(alpha), alpha_ex.1);
                self._synth_call(substituted_ty, arg)
            },
        }
    }

    fn synth_item(&mut self, item: ItemId) -> TyResult<Ty> {
        let item = self.hir.item(item);

        match item.kind() {
            ItemKind::TyAlias(_ty) => {
                // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                self.conv_ty_alias(item.def_id());
            },
            ItemKind::Mod(Mod { items }) => {
                for item in items {
                    self.synth_item(*item)?;
                }
            },
            ItemKind::Decl(decl) => {
                let value_ty = self.synth_expr(decl.value)?;
                assert!(self.def_types.insert(item.def_id(), value_ty).is_none());
                self.tyctx.type_term(item.name(), value_ty)
            },
        }

        TyResult::Ok(self.tyctx.unit())
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

        Ok(self.tyctx.unit())
    }

    // Check //
    fn check(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        let expr = self.hir.expr(expr_id);
        let tys = self.tyctx.ty(ty);

        match (&expr.kind(), tys.kind()) {
            (ExprKind::Lit(lit), TyKind::Lit(prim)) => {
                if *prim != PrimTy::from(*lit) {
                    MessageBuilder::error()
                        .span(expr.span())
                        .text(format!("Type mismatch: expected {}", prim))
                        .label(expr.span(), format!("Must be of type {}", prim))
                        .emit(self);
                    return Err(TyError());
                }
                Ok(ty)
            },

            (ExprKind::Lambda(lambda), &TyKind::Func(param_ty, body_ty)) => {
                let param_name = self.hir.pat_names(lambda.param).unwrap();
                assert!(param_name.len() == 1);
                let param_name = param_name[0];

                self.under_ctx(Ctx::new_with_term(param_name, param_ty), |this| {
                    this.check(lambda.body, body_ty)
                })
            },

            (_, &TyKind::Forall(alpha, body)) => self.under_ctx(Ctx::new_with_var(alpha), |this| {
                this.check(expr_id, body)?;
                Ok(ty)
            }),

            _ => {
                let expr_ty = self.synth_expr(expr_id)?;
                let l = self.tyctx.apply_on(expr_ty);
                let r = self.tyctx.apply_on(ty);

                match self.subtype(l, r) {
                    Ok(()) => {},
                    Err(_) => {
                        MessageBuilder::error()
                            .span(expr.span())
                            .text(format!(
                                "{} is not a subtype of {}",
                                self.tyctx.ty(l),
                                self.tyctx.ty(r)
                            ))
                            .label(
                                expr.span(),
                                format!(
                                    "{} is not a subtype of {}",
                                    self.tyctx.ty(l),
                                    self.tyctx.ty(r)
                                ),
                            )
                            .emit(self);
                    },
                }

                Ok(ty)
            },
        }
    }

    // Subtyping //
    fn subtype(&mut self, l_ty: Ty, r_ty: Ty) -> TyResult<()> {
        assert!(self.tyctx.ty_wf(l_ty).is_ok() && self.tyctx.ty_wf(r_ty).is_ok());

        match (self.tyctx.ty(l_ty).kind(), self.tyctx.ty(r_ty).kind()) {
            (TyKind::Lit(kind), TyKind::Lit(kind_)) if kind == kind_ => Ok(()),

            (TyKind::Var(name), TyKind::Var(name_)) if name.sym() == name_.sym() => Ok(()),

            (TyKind::Existential(ex1), TyKind::Existential(ex2)) if ex1 == ex2 => Ok(()),

            (&TyKind::Func(param, ret), &TyKind::Func(param_, ret_)) => {
                self.under_new_ctx(|this| {
                    // Enter Θ
                    this.subtype(param, param_)?;
                    let ret = this.tyctx.apply_on(ret);
                    let ret_ = this.tyctx.apply_on(ret_);
                    this.subtype(ret, ret_)
                })
            },

            (&TyKind::Forall(alpha, body), _) => {
                let ex = self.tyctx.fresh_ex();
                let ex_ty = self.tyctx.existential(ex);
                let with_substituted_alpha = self.tyctx.substitute(body, Subst::Name(alpha), ex_ty);

                self.under_ctx(Ctx::new_with_ex(ex), |this| {
                    this.subtype(with_substituted_alpha, r_ty)
                })
            },

            (_, &TyKind::Forall(alpha, body)) => {
                self.under_ctx(Ctx::new_with_var(alpha), |this| this.subtype(l_ty, body))
            },

            (&TyKind::Existential(ex), _) => {
                if !self.tyctx.occurs_in(r_ty, Subst::Existential(ex)) {
                    self.instantiate_l(ex, r_ty)
                } else {
                    Err(TyError())
                }
            },

            (_, &TyKind::Existential(ex)) => {
                if !self.tyctx.occurs_in(l_ty, Subst::Existential(ex)) {
                    self.instantiate_r(l_ty, ex)
                } else {
                    Err(TyError())
                }
            },

            _ => Err(TyError()),
        }
    }

    fn instantiate_l(&mut self, ex: ExistentialId, r_ty: Ty) -> TyResult<()> {
        let ty = self.tyctx.ty(r_ty);

        // InstLReach
        match ty.kind() {
            &TyKind::Existential(ty_ex) => {
                let ex_depth = self.tyctx.find_unbound_ex_depth(ex);
                let ty_ex_depth = self.tyctx.find_unbound_ex_depth(ty_ex);
                if ex_depth < ty_ex_depth {
                    let ex_ty = self.tyctx.existential(ex);
                    self.tyctx.solve(ty_ex, ex_ty);
                    return Ok(());
                }
            },
            _ => {},
        }

        // InstLSolve
        if self.tyctx.is_mono(r_ty) {
            // FIXME: check WF?
            self.tyctx.solve(ex, r_ty);
            return Ok(());
        }

        match ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Lit(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            &TyKind::Func(param, body) => self.try_to(|this| {
                let domain_ex = this.add_fresh_ex();
                let range_ex = this.add_fresh_ex();

                let func_ty = this.tyctx.func(domain_ex.1, range_ex.1);

                this.tyctx.solve(ex, func_ty);

                this.instantiate_r(param, domain_ex.0)?;

                let range_ty = this.tyctx.apply_on(body);
                this.instantiate_l(range_ex.0, range_ty)
            }),
            &TyKind::Forall(alpha, ty) => self.try_to(|this| {
                this.under_ctx(Ctx::new_with_var(alpha), |this| this.instantiate_l(ex, ty))
            }),
        }
    }

    fn instantiate_r(&mut self, l_ty: Ty, ex: ExistentialId) -> TyResult<()> {
        let ty = self.tyctx.ty(l_ty);

        // InstRReach
        match ty.kind() {
            &TyKind::Existential(ty_ex) => {
                let ex_depth = self.tyctx.find_unbound_ex_depth(ex);
                let ty_ex_depth = self.tyctx.find_unbound_ex_depth(ty_ex);
                if ex_depth < ty_ex_depth {
                    let ex_ty = self.tyctx.existential(ex);
                    self.tyctx.solve(ty_ex, ex_ty);
                    return Ok(());
                }
            },
            _ => {},
        }

        if self.tyctx.is_mono(l_ty) {
            // FIXME: Check WF?
            self.tyctx.solve(ex, l_ty);
            return Ok(());
        }

        match ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Lit(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            &TyKind::Func(param, body) => self.try_to(|this| {
                let domain_ex = this.add_fresh_ex();
                let range_ex = this.add_fresh_ex();

                let func_ty = this.tyctx.func(domain_ex.1, range_ex.1);

                this.tyctx.solve(ex, func_ty);

                this.instantiate_l(domain_ex.0, param)?;

                let range_ty = this.tyctx.apply_on(body);
                this.instantiate_r(range_ty, range_ex.0)
            }),
            &TyKind::Forall(alpha, ty) => self.try_to(|this| {
                let alpha_ex = this.tyctx.fresh_ex();

                this.under_ctx(Ctx::new_with_ex(alpha_ex), |this| {
                    let alpha_ex_ty = this.tyctx.existential(alpha_ex);
                    let body_ty = this.tyctx.substitute(ty, Subst::Name(alpha), alpha_ex_ty);
                    this.instantiate_r(body_ty, ex)
                })
            }),
        }
    }
}

impl<'ast> Stage<()> for Typecker<'ast> {
    fn run(self) -> StageOutput<()> {
        StageOutput::new(self.sess, (), self.msg)
    }
}
