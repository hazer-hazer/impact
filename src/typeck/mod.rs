use crate::{
    hir::{
        self,
        expr::{Block, ExprKind, Lit, TyExpr},
        item::{ItemId, ItemKind, Mod, TyAlias},
        HIR,
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
            msg: Default::default(),
        }
    }

    fn ctx(&mut self) -> &mut Ctx {
        self.tyctx.ctx()
    }

    pub fn under_ctx<T>(&mut self, ctx: Ctx, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.tyctx.enter_ctx(ctx);
        let res = f(self);
        self.tyctx.exit_ctx();
        res
    }

    pub fn under_new_ctx<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.under_ctx(Ctx::default(), f)
    }

    // Conversion //
    fn conv(&mut self, ty: hir::ty::Ty) -> Ty {
        let ty = self.hir.ty(ty);
        // TODO: Allow recursive?

        match ty.kind {
            hir::ty::TyKind::Unit => self.tyctx.unit(),
            hir::ty::TyKind::Path(path) => self.conv_path(path),
            hir::ty::TyKind::Func(_, _) => todo!(),
        }
    }

    fn conv_path(&mut self, path: hir::Path) -> Ty {
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
    fn synth_expr(&mut self, expr: hir::expr::Expr) -> TyResult<Ty> {
        let expr = self.hir.expr(expr);
        match &expr.kind {
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
            ExprKind::Block(_) => todo!(),
            ExprKind::Infix(_) => todo!(),
            ExprKind::Prefix(_) => todo!(),
            ExprKind::Lambda(_) => todo!(),
            ExprKind::Call(_) => todo!(),
            ExprKind::Let(_) => todo!(),
            ExprKind::Ty(TyExpr { expr, ty: anno }) => {
                let ty = self.conv(*anno);
                self.check(*expr, ty)?;
                Ok(ty)
            },
        }
    }

    fn synth_path(&self, path: hir::Path) -> TyResult<Ty> {
        let path = self.hir.path(path);
        self.tyctx
            .lookup_typed_term_ty(path.target_name())
            .ok_or(TyError())
    }

    fn synth_ty_expr(&self, _ty_expr: &TyExpr) -> TyResult<(Ty, Ctx)> {
        todo!()
    }

    fn synth_block(&self, _block: Block) -> TyResult<(Ty, Ctx)> {
        todo!()
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
                self.ctx().type_term(item.name(), value_ty)
            },
        }

        TyResult::Ok(self.tyctx.unit())
    }

    // Check //
    fn check(&mut self, expr_id: hir::expr::Expr, ty: Ty) -> TyResult<()> {
        let expr = self.hir.expr(expr_id);
        let tys = self.tyctx.ty(ty);

        match (&expr.kind, tys.kind()) {
            (ExprKind::Lit(lit), TyKind::Lit(prim)) => {
                if *prim != PrimTy::from(*lit) {
                    MessageBuilder::error()
                        .span(expr.span())
                        .text(format!("Type mismatch: expected {}", prim))
                        .label(expr.span(), format!("Must be of type {}", prim))
                        .emit(self);
                    return Err(TyError());
                }
                Ok(())
            },

            (ExprKind::Lambda(_lambda), TyKind::Func(_param_ty, _ret_ty)) => {
                // let typed_param = CtxItem::TypedTerm(lambda.param.as_ref().unwrap(), *param_ty);
                // self.tyctx.ctx().add(typed_param);

                // Patterns :(
                todo!()
            },

            (_, &TyKind::Forall(alpha, body)) => self.under_ctx(Ctx::new_with_var(alpha), |this| {
                this.check(expr_id, body)?;
                Ok(())
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

                Ok(())
            },
        }
    }

    // Subtyping //
    fn subtype(&mut self, l_ty: Ty, r_ty: Ty) -> TyResult<()> {
        assert!(self.tyctx.ty_wf(l_ty).is_ok() && self.tyctx.ty_wf(r_ty).is_ok());

        match (self.tyctx.ty(l_ty).kind(), self.tyctx.ty(r_ty).kind()) {
            (TyKind::Lit(kind), TyKind::Lit(kind_)) if kind == kind_ => Ok(()),

            (TyKind::Var(name), TyKind::Var(name_)) if name.sym() == name_.sym() => Ok(()),

            (TyKind::Existential(id), TyKind::Existential(id_)) if id == id_ => Ok(()),

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

            (&TyKind::Existential(id), _) => {
                if !self.tyctx.occurs_in(r_ty, Subst::Existential(id)) {
                    todo!("InstantiateL")
                }
                Err(TyError())
            },

            (_, &TyKind::Existential(id)) => {
                if !self.tyctx.occurs_in(l_ty, Subst::Existential(id)) {
                    todo!("InstantiateR")
                }
                Err(TyError())
            },

            _ => Err(TyError()),
        }
    }

    fn instantiate_l(&mut self, ex: ExistentialId, r_ty: Ty) -> TyResult<()> {
        let ex_depth = self.tyctx.find_unbound_ex_depth(ex);

        let ty = self.tyctx.ty(r_ty);

        if self.tyctx.is_mono(r_ty) {
            match ty.kind() {
                &TyKind::Existential(ty_ex) => {
                    if ex_depth < self.tyctx.find_unbound_ex_depth(ty_ex) {
                        return Ok(self.tyctx.ctx().solve(ex, r_ty));
                    }
                },
                _ => {},
            }
        }

        match self.tyctx.ty(r_ty).kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Lit(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => unreachable!("Unchecked monotype in `instantiate_l`"),
            TyKind::Func(param, body) => {
                let alpha1 = self.tyctx.fresh_ex();
                let alpha2 = self.tyctx.fresh_ex();
            },
            TyKind::Forall(_, _) => todo!(),
        }
    }
}

impl<'ast> Stage<()> for Typecker<'ast> {
    fn run(self) -> StageOutput<()> {
        StageOutput::new(self.sess, (), self.msg)
    }
}
