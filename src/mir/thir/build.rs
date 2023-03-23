use crate::{
    hir::{self, OwnerId, WithHirId, HIR},
    resolve::{builtin::Builtin, def::DefKind},
    span::span::WithSpan,
    typeck::tyctx::TyCtx,
};

use super::{
    Block, BlockId, Expr, ExprId, ExprKind, Lit, LocalVar, Param, ParamId, Pat, PatKind, Stmt,
    StmtId, THIR,
};

pub struct ThirBuilder<'ctx> {
    hir: &'ctx HIR,
    tyctx: &'ctx TyCtx,

    thir: THIR,
}

impl<'ctx> ThirBuilder<'ctx> {
    pub fn new(hir: &'ctx HIR, tyctx: &'ctx TyCtx, body_owner: OwnerId) -> Self {
        Self {
            hir,
            tyctx,
            thir: THIR::new(body_owner),
        }
    }

    /// Build THIR from HIR body, returning THIR and entry ExprId
    pub fn build_body_thir(mut self) -> (THIR, ExprId) {
        let body = self
            .hir
            .body(self.hir.owner_body(self.thir.body_owner).unwrap());

        body.param.map(|param| self.param(param));
        let expr = self.expr(body.value);

        (self.thir, expr)
    }

    fn param(&mut self, param: hir::pat::Pat) -> ParamId {
        let pat = self.pat(param);
        self.thir.add_param(Param { pat })
    }

    fn expr(&mut self, expr_id: hir::expr::Expr) -> ExprId {
        let expr = self.hir.expr(expr_id);
        let kind = match expr.kind() {
            hir::expr::ExprKind::Lit(lit) => {
                let lit = match *lit {
                    hir::expr::Lit::Bool(val) => Lit::Bool(val),
                    hir::expr::Lit::Int(val, _) => Lit::Int(val, self.tyctx.tyof(expr_id).as_int()),
                    hir::expr::Lit::Float(val, _) => {
                        Lit::Float(val, self.tyctx.tyof(expr_id).as_float_kind())
                    },
                    hir::expr::Lit::String(val) => Lit::String(val),
                };
                ExprKind::Lit(lit)
            },
            hir::expr::ExprKind::Path(path) => match self.hir.path(path.0).res() {
                &hir::Res::Def(kind, def_id) => match kind {
                    DefKind::Func | DefKind::Value => {
                        ExprKind::Def(def_id, self.tyctx.tyof(expr_id))
                    },
                    DefKind::Lambda => unreachable!("Lambda cannot be resolved by path"),
                    DefKind::TyAlias | DefKind::DeclareBuiltin | DefKind::Root | DefKind::Mod => {
                        unreachable!("Expression path cannot point to such definition kinds")
                    },
                },
                &hir::Res::Local(hir_id) => ExprKind::LocalRef(LocalVar::new(hir_id)),
                hir::Res::Builtin(_) | hir::Res::DeclareBuiltin | hir::Res::Error => unreachable!(),
            },
            &hir::expr::ExprKind::Block(block) => ExprKind::Block(self.block(block)),
            &hir::expr::ExprKind::Lambda(hir::expr::Lambda {
                body_id: body,
                def_id,
            }) => ExprKind::Lambda {
                body_id: body,
                def_id,
            },
            &hir::expr::ExprKind::Call(hir::expr::Call { lhs, arg }) => ExprKind::Call {
                lhs: self.expr(lhs),
                arg: self.expr(arg),
                func_ty: self.tyctx.instantiated_expr_ty(lhs).unwrap(),
            },
            &hir::expr::ExprKind::Let(block) => ExprKind::Block(self.block(block)),
            &hir::expr::ExprKind::Ty(hir::expr::TyExpr { expr, ty: _ }) => {
                ExprKind::Ty(self.expr(expr), self.tyctx.tyof(expr_id))
            },
            &hir::expr::ExprKind::BuiltinExpr(bt) => ExprKind::Builtin(bt),
        };

        self.thir.add_expr(Expr {
            ty: self.tyctx.tyof(expr_id),
            span: expr.span(),
            kind,
        })
    }

    // fn call(&mut self, call: &hir::expr::Call) -> ExprKind {
    //     let lhs = call.lhs;
    //     let arg = call.arg;

    //     match self.hir.expr(lhs).kind() {
    //         hir::expr::ExprKind::BuiltinExpr(bt) => match bt {
    //             Builtin::AddInt => todo!(),
    //             Builtin::SubInt => todo!(),
    //             _ => {},
    //         },
    //         _ => {},
    //     }

    //     ExprKind::Call {
    //         lhs: self.expr(lhs),
    //         arg: self.expr(arg),
    //         func_ty: self.tyctx.instantiated_expr_ty(lhs).unwrap(),
    //     }
    // }

    fn stmt(&mut self, stmt_id: hir::stmt::Stmt) -> Option<StmtId> {
        let stmt = self.hir.stmt(stmt_id);

        let stmt = match stmt.kind() {
            &hir::stmt::StmtKind::Expr(expr) => Some(Stmt::Expr(self.expr(expr))),
            hir::stmt::StmtKind::Item(_) => None,
        };

        stmt.map(|stmt| self.thir.add_stmt(stmt))
    }

    fn block(&mut self, block_id: hir::expr::Block) -> BlockId {
        let block = self.hir.block(block_id);

        let stmts = block
            .stmts()
            .iter()
            .filter_map(|&stmt| self.stmt(stmt))
            .collect();
        let expr = block.expr().map(|&expr| self.expr(expr));

        self.thir.add_block(Block { stmts, expr })
    }

    fn pat(&mut self, pat: hir::pat::Pat) -> Pat {
        let pat = self.hir.pat(pat);
        let ty = self.tyctx.tyof(pat.id());

        let kind = match pat.kind() {
            hir::pat::PatKind::Unit => PatKind::Unit,
            &hir::pat::PatKind::Ident(ident) => PatKind::Ident {
                name: ident,
                var: LocalVar::new(pat.id()),
                ty,
            },
        };

        Pat {
            ty,
            kind,
            span: pat.span(),
        }
    }
}
