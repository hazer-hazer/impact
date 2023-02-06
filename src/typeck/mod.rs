use crate::{
    hir::{
        self,
        expr::{Block, ExprKind, Lit, TyExpr},
        item::{ItemId, ItemKind, Mod},
        HIR,
    },
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{
        def::{DefKind, DefMap},
        res::ResKind,
    },
    session::{Session, Stage, StageOutput},
    span::span::WithSpan,
};

use self::{
    ctx::Ctx,
    ty::{FloatKind, IntKind, PrimTy, Ty, TyCtx, TyError, TyKind, TyResult},
};

pub mod ctx;
pub mod ty;

struct Typecker<'hir> {
    tyctx: TyCtx,

    hir: &'hir HIR,
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
                if let Some(def_ty) = self.def_types.get_flat(def_id) {
                    return *def_ty;
                }

                let def = self.sess.def_table.get_def(def_id).unwrap();
                match def.kind() {
                    DefKind::TyAlias => {
                        let ty_alias = self.hir.item(ItemId::new(def.def_id.into()));
                        let ty = match ty_alias.kind() {
                            ItemKind::Type(ty_item) => ty_item.ty,
                            _ => unreachable!(),
                        };
                        let ty = self.conv(ty);
                        self.def_types.insert(def_id, ty);
                        ty
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
            ItemKind::Type(_ty) => {
                todo!()
            },
            ItemKind::Mod(Mod { items }) => {
                for item in items {
                    self.synth_item(*item)?;
                }
            },
            ItemKind::Decl(decl) => {
                let value_ty = self.synth_expr(decl.value)?;
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

            (_, &TyKind::Forall(alpha, body)) => {
                self.tyctx.under_ctx(Ctx::new_with_var(alpha), |ctx| {
                    self.check(expr_id, body)?;
                    Ok(())
                })
            },

            _ => {
                let expr_ty = self.synth_expr(expr_id)?;
                let l = self.tyctx.apply_ctx(expr_ty);
                let r = self.tyctx.apply_ctx(ty);

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
                self.subtype(param, param_)?;
                let ret = self.tyctx.apply_ctx(ret);
                let ret_ = self.tyctx.apply_ctx(ret_);
                self.subtype(ret, ret_)
            },

            (&TyKind::Forall(_alpha, _body), _) => {
                todo!()
            },

            (_, &TyKind::Forall(_alpha, _body)) => {
                todo!()
            },

            (&TyKind::Existential(_id), _) => {
                todo!()
            },

            (_, &TyKind::Existential(_id)) => {
                todo!()
            },

            _ => Err(TyError()),
        }
    }
}

impl<'ast> Stage<()> for Typecker<'ast> {
    fn run(self) -> StageOutput<()> {
        StageOutput::new(self.sess, (), self.msg)
    }
}
