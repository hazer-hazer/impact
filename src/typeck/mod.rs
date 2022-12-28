use crate::{
    ast::{
        self,
        expr::{Block, Expr, ExprKind, Lit, TyExpr},
        item::{Item, ItemKind},
        MappedAst, NodeId, Path, WithNodeId,
    },
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    parser::token,
    resolve::{
        def::{DefKind, DefMap},
        res::{NamePath, ResKind},
    },
    session::{Session, Stage, StageOutput},
    span::span::{Spanned, WithSpan},
};

use self::{
    ctx::{Ctx, CtxItem},
    ty::{FloatKind, IntKind, PrimTy, Ty, TyCtx, TyError, TyKind, TyResult},
};

pub mod ctx;
pub mod ty;

struct Typecker<'ast> {
    tyctx: TyCtx,

    ast: MappedAst<'ast>,

    def_types: DefMap<Ty>,

    msg: MessageStorage,
    sess: Session,
}

impl<'ast> MessageHolder for Typecker<'ast> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'ast> Typecker<'ast> {
    // Conversion //
    fn conv(&mut self, ty_node_id: NodeId) -> Ty {
        // TODO: Allow recursive?

        let ty = self.ast.map().ty(ty_node_id);
        match ty.kind() {
            ast::ty::TyKind::Unit => self.tyctx.unit(),
            ast::ty::TyKind::Path(path) => {
                let path_node_id = path.as_ref().unwrap().id();
                self.conv_path(path_node_id)
            },
            ast::ty::TyKind::Func(_, _) => todo!(),
            ast::ty::TyKind::Paren(_) => todo!(),
        }
    }

    fn conv_path(&mut self, path_node_id: NodeId) -> Ty {
        let path = self.ast.map().path(path_node_id);
        let res = self.sess.res.get(NamePath::new(path.id())).unwrap();
        match res.kind() {
            &ResKind::Def(def_id) => {
                if let Some(def_ty) = self.def_types.get(&def_id) {
                    return *def_ty;
                }

                let def = self.sess.def_table.get_def(def_id).unwrap();
                match def.kind() {
                    DefKind::TyAlias => {
                        let ty_alias = self
                            .ast
                            .map()
                            .item(self.sess.def_table.get_node_id(def_id).unwrap());
                        let ty_node_id = match ty_alias.kind() {
                            ItemKind::Type(_, ty) => ty.as_ref().unwrap().id(),
                            _ => unreachable!(),
                        };
                        let ty = self.conv(ty_node_id);
                        self.def_types.insert(def_id, ty);
                        ty
                    },

                    // Non-type definitions from type namespace
                    DefKind::Mod => {
                        MessageBuilder::error()
                            .span(path.span())
                            .text(format!("{} item used as type", def.kind()))
                            .emit_single_label(self);

                        self.tyctx.error()
                    },

                    // Definitions from value namespace
                    DefKind::Func => unreachable!(),
                }
            },
            _ => unreachable!(),
        }
    }

    fn conv_int_kind(&self, kind: token::IntKind) -> IntKind {
        match kind {
            token::IntKind::Unknown => todo!(),
            token::IntKind::U8 => IntKind::U8,
            token::IntKind::U16 => IntKind::U16,
            token::IntKind::U32 => IntKind::U32,
            token::IntKind::U64 => IntKind::U64,
            token::IntKind::Uint => IntKind::Uint,
            token::IntKind::I8 => IntKind::I8,
            token::IntKind::I16 => IntKind::I16,
            token::IntKind::I32 => IntKind::I32,
            token::IntKind::I64 => IntKind::I64,
            token::IntKind::Int => IntKind::Int,
        }
    }

    fn conv_float_kind(&self, kind: token::FloatKind) -> FloatKind {
        match kind {
            token::FloatKind::Unknown => todo!(),
            token::FloatKind::F32 => FloatKind::F32,
            token::FloatKind::F64 => FloatKind::F64,
        }
    }

    // Synthesis //
    fn synth_expr(&mut self, expr: &Expr) -> TyResult<Ty> {
        match expr.kind() {
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
            ExprKind::Path(path) => self.synth_path(path.0.as_ref().unwrap()),
            ExprKind::Block(_) => todo!(),
            ExprKind::Infix(_) => todo!(),
            ExprKind::Prefix(_) => todo!(),
            ExprKind::Lambda(_) => todo!(),
            ExprKind::Call(_) => todo!(),
            ExprKind::Let(_) => todo!(),
            ExprKind::Ty(TyExpr { expr, ty: anno }) => {
                let ty = self.conv(anno.as_ref().unwrap().id());
                self.check(expr.as_ref().unwrap(), ty)?;
                Ok(ty)
            },
        }
    }

    fn synth_path(&self, path: &Path) -> TyResult<Ty> {
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

    fn synth_item(&self, item: &Item) -> TyResult<Ty> {
        match item.kind() {
            ItemKind::Type(name, ty) => {
                todo!()
            },
            ItemKind::Mod(name, items) => todo!(),
            ItemKind::Decl(name, params, body) => todo!(),
        }
    }

    // Check //
    fn check(&mut self, expr: &Expr, ty: Ty) -> TyResult<()> {
        let tys = self.tyctx.ty(ty);

        match (expr.kind(), tys.kind()) {
            (ExprKind::Lit(ast_lit), TyKind::Lit(prim)) => {
                if *prim != PrimTy::from(ast_lit) {
                    MessageBuilder::error()
                        .span(expr.span())
                        .text(format!("Type mismatch: expected {}", prim))
                        .label(expr.span(), format!("Must be of type {}", prim))
                        .emit(self);
                    return Err(TyError());
                }
                Ok(())
            },

            (ExprKind::Lambda(lambda), TyKind::Func(param_ty, ret_ty)) => {
                // let typed_param = CtxItem::TypedTerm(lambda.param.as_ref().unwrap(), *param_ty);
                // self.tyctx.ctx().add(typed_param);

                // Patterns :(
                todo!()
            },

            (_, &TyKind::Forall(alpha, body)) => {
                let alpha_item = CtxItem::Var(alpha);
                self.tyctx.ctx().enter(alpha, vec![alpha_item]);
                self.check(expr, body)?;
                self.tyctx.ctx().leave(alpha);
                Ok(())
            },

            _ => {
                let expr_ty = self.synth_expr(expr)?;
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

            (&TyKind::Forall(alpha, body), _) => {
                todo!()
            },

            (_, &TyKind::Forall(alpha, body)) => {
                todo!()
            },

            (&TyKind::Existential(id), _) => {
                todo!()
            },

            (_, &TyKind::Existential(id)) => {
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
