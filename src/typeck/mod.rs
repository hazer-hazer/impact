use crate::{
    ast::{
        self,
        expr::{Block, Expr, ExprKind, Lit, TyExpr},
        MappedAst, Path, WithNodeId,
    },
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    parser::token,
    resolve::{
        def::{DefKind, DefMap},
        res::{NamePath, ResKind},
    },
    session::{Session, Stage, StageOutput},
    span::span::WithSpan,
};

use self::{
    ctx::{Ctx, CtxItem},
    ty::{FloatKind, IntKind, PrimTy, Ty, TyCtx, TyError, TyResult},
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
    fn conv(&mut self, ty: &ast::ty::Ty) -> Ty {
        match ty.kind() {
            ast::ty::TyKind::Unit => self.tyctx.unit(),
            ast::ty::TyKind::Path(path) => self.conv_path(path.as_ref().unwrap()),
            ast::ty::TyKind::Func(_, _) => todo!(),
            ast::ty::TyKind::Paren(_) => todo!(),
        }
    }

    fn conv_path(&mut self, path: &ast::Path) -> Ty {
        let res = self.sess.res.get(NamePath::new(path.id())).unwrap();
        match res.kind() {
            &ResKind::Def(def_id) => {
                let def = self.sess.def_table.get_def(def_id).unwrap();
                match def.kind() {
                    DefKind::TyAlias => {
                        let ty_alias = self
                            .ast
                            .map()
                            .item(self.sess.def_table.get_node_id(def_id).unwrap());
                        let ty = match ty_alias.kind() {
                            ast::item::ItemKind::Type(_, ty) => ty.as_ref().unwrap(),
                            _ => unreachable!(),
                        };
                        let ty = self.conv(ty);
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
    pub fn synth(&mut self, expr: &Expr) -> TyResult<Ty> {
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
            ExprKind::Ty(_) => todo!(),
        }
    }

    fn synth_path(&self, path: &Path) -> TyResult<Ty> {
        if let Some(item) = self.tyctx.lookup_typed_term(path.target_name()) {
            Ok(match item {
                CtxItem::TypedTerm(_, ty) => ty.clone(),
                _ => unreachable!(),
            })
        } else {
            Err(TyError())
        }
    }

    fn synth_ty_expr(&self, _ty_expr: &TyExpr) -> TyResult<(Ty, Ctx)> {
        todo!()
    }

    fn synth_block(&self, _block: Block) -> TyResult<(Ty, Ctx)> {
        todo!()
    }
}

impl<'ast> Stage<()> for Typecker<'ast> {
    fn run(self) -> StageOutput<()> {
        StageOutput::new(self.sess, (), self.msg)
    }
}
