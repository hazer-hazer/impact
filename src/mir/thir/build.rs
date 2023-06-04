use super::{
    Block, BlockId, Expr, ExprId, ExprKind, Lit, LocalVar, Param, ParamId, Pat, PatKind, Stmt,
    StmtId, THIR,
};
use crate::{
    hir::{self, ExprDefKind, Map, OwnerId, WithHirId, HIR},
    resolve::builtin::ValueBuiltin,
    session::{impl_session_holder, Session, SessionHolder},
    span::WithSpan,
    typeck::tyctx::TyCtx,
};

pub struct ThirBuilder<'ctx> {
    hir: &'ctx HIR,
    sess: &'ctx Session,
    thir: THIR,
}

impl_session_holder!(ThirBuilder<'ctx>);

impl<'ctx> ThirBuilder<'ctx> {
    pub fn new(sess: &'ctx Session, hir: &'ctx HIR, body_owner: OwnerId) -> Self {
        Self {
            hir,
            sess,
            thir: THIR::new(body_owner),
        }
    }

    /// Build THIR from HIR body, returning THIR and entry ExprId
    pub fn build_body_thir(mut self) -> (THIR, ExprId) {
        let body = self
            .hir
            .body(self.hir.owner_body(self.thir.body_owner).unwrap());

        body.params.iter().copied().for_each(|param| {
            self.param(param);
        });
        let expr = self.expr(body.value);

        (self.thir, expr)
    }

    fn param(&mut self, param: hir::Pat) -> ParamId {
        let pat = self.pat(param);
        self.thir.add_param(Param { pat })
    }

    fn expr(&mut self, expr_id: hir::Expr) -> ExprId {
        let expr = self.hir.expr(expr_id);
        let kind = match expr.kind() {
            hir::expr::ExprKind::Lit(lit) => {
                let lit = match *lit {
                    hir::expr::Lit::Bool(val) => Lit::Bool(val),
                    hir::expr::Lit::Int(val, _) => {
                        Lit::Int(val, self.tyctx().tyof(expr_id).as_int().unwrap())
                    },
                    hir::expr::Lit::Float(val, _) => {
                        Lit::Float(val, self.tyctx().tyof(expr_id).as_float_kind().unwrap())
                    },
                    hir::expr::Lit::String(val) => Lit::String(val),
                };
                ExprKind::Lit(lit)
            },
            &hir::expr::ExprKind::Path(path) => match self.hir.expr_path(path).res() {
                &hir::ExprRes::Def(kind, def_id) => match kind {
                    ExprDefKind::Ctor
                    | ExprDefKind::FieldAccessor
                    | ExprDefKind::External
                    | ExprDefKind::Func
                    | ExprDefKind::Value => ExprKind::Def(def_id, self.tyctx().tyof(expr_id)),
                },
                &hir::ExprRes::Local(hir_id) => ExprKind::LocalRef(LocalVar::new(hir_id)),
            },
            &hir::expr::ExprKind::Block(block) => ExprKind::Block(self.block(block)),
            &hir::expr::ExprKind::Lambda(hir::expr::Lambda {
                body_id: body,
                def_id,
            }) => ExprKind::Lambda {
                body_id: body,
                def_id,
            },
            hir::expr::ExprKind::Call(call) => self.call(call),
            &hir::expr::ExprKind::Let(block) => ExprKind::Block(self.block(block)),
            &hir::expr::ExprKind::Ty(hir::expr::TyExpr { expr, ty: _ }) => {
                ExprKind::Ty(self.expr(expr), self.tyctx().tyof(expr_id))
            },
            // &hir::expr::ExprKind::FieldAccess(lhs, _) => {
            //     // TODO: Maybe add field name?
            //     ExprKind::FieldAccess(
            //         self.expr(lhs),
            //         self.tyctx().field_index(expr_id).unwrap(),
            //         VariantId::new(0),
            //     )
            // },
            &hir::expr::ExprKind::Builtin(bt) => ExprKind::Builtin(bt),
            hir::expr::ExprKind::Match(..) => todo!(),
        };

        self.thir.add_expr(Expr {
            ty: self.tyctx().tyof(expr_id),
            span: expr.span(),
            kind,
        })
    }

    fn call(&mut self, call: &hir::expr::Call) -> ExprKind {
        let lhs = call.lhs;
        let args = &call.args;

        match self.hir.expr(lhs).kind() {
            hir::expr::ExprKind::Builtin(bt) => match bt {
                ValueBuiltin::RefCons => {
                    assert_eq!(args.len(), 1);
                    let arg = self.expr(args[0]);
                    return ExprKind::Ref(arg);
                },
                _ => {},
            },
            _ => {},
        }

        ExprKind::Call {
            lhs: self.expr(lhs),
            args: args.iter().copied().map(|arg| self.expr(arg)).collect(),
            func_ty: self.tyctx().instantiated_expr_ty(lhs).unwrap(),
        }
    }

    fn stmt(&mut self, stmt_id: hir::Stmt) -> Option<StmtId> {
        let stmt = self.hir.stmt(stmt_id);

        let stmt = match stmt.kind() {
            &hir::stmt::StmtKind::Expr(expr) => Some(Stmt::Expr(self.expr(expr))),
            hir::stmt::StmtKind::Local(local) => Some(self.local(local)),
            hir::stmt::StmtKind::Item(_) => None,
        };

        stmt.map(|stmt| self.thir.add_stmt(stmt))
    }

    fn local(&mut self, local: &hir::stmt::Local) -> Stmt {
        let ty = self.tyctx().tyof(local.id());
        Stmt::Local(self.pat(local.pat), self.expr(local.value))
    }

    fn block(&mut self, block_id: hir::Block) -> BlockId {
        let block = self.hir.block(block_id);

        let stmts = block
            .stmts()
            .iter()
            .copied()
            .filter_map(|stmt| self.stmt(stmt))
            .collect();
        let expr = block.expr().map(|expr| self.expr(expr));

        self.thir.add_block(Block { stmts, expr })
    }

    fn pat(&mut self, pat: hir::Pat) -> Pat {
        let pat = self.hir.pat(pat);
        let ty = self.tyctx().tyof(pat.id());

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
