use std::collections::HashMap;

use crate::{
    hir::{
        expr::{ExprKind, TyExpr},
        pat::PatKind,
        Block, BodyId, Expr, Pat, HIR,
    },
    interface::writer::{out, Writer},
    pp::AstLikePP,
    typeck::ty::Ty,
};

pub enum InferEntryKind {
    Try(String),
    Body(BodyId),
}

pub struct InferEntry {
    kind: InferEntryKind,
    exprs: Vec<(Expr, Vec<InferStep>)>,
}

pub enum InferStep {
    Synthesized(Ty),

    /// Before and after context application
    CtxApplied(Ty, Ty),
}

pub struct InferDebug<'hir> {
    hir: &'hir HIR,
    pp: AstLikePP<'hir>,
}

impl<'hir> InferDebug<'hir> {
    fn entry(&mut self, entry: &InferEntry) {
        match entry.kind {
            InferEntryKind::Try(to) => self.pp.string(format!("Try to {to}")),
            InferEntryKind::Body(body) => ,
        }
        self.pp.indent();
        self.pp.dedent();
    }

    fn expr(&mut self, expr: Expr, steps: &[InferStep]) {
        self.pp_expr(expr);
        self.pp.indent();
        self.pp.out_indent();
        steps.iter().for_each(|step| self.step(step));
        self.pp.dedent();
    }

    fn step(&mut self, step: &InferStep) {
        match step {
            InferStep::Synthesized(ty) => {
                self.pp.string(format!("Synthesized {ty}"));
            },
            InferStep::CtxApplied(..) => todo!(),
        }
    }

    fn pp_expr(&mut self, expr: Expr) -> &mut AstLikePP<'hir> {
        match self.hir.expr(expr).kind() {
            ExprKind::Lit(lit) => self.pp.string(lit),
            &ExprKind::Path(path) => self.pp.string(self.hir.expr_path(path)),
            &ExprKind::Block(block) => self.pp_block(block),
            ExprKind::Lambda(lambda) => self.pp_body(lambda.body_id),
            ExprKind::Call(call) => {
                self.pp_expr(call.lhs).sp();
                call.args.iter().copied().for_each(|arg| {
                    self.pp_expr(arg);
                });
                &mut self.pp
            },
            &ExprKind::Let(block) => self.pp_block(block),
            ExprKind::Ty(ty_expr) => self.pp_expr(ty_expr.expr).str(": [ty]"),
            &ExprKind::Builtin(bt) => self.pp.string(bt),
        }
    }

    fn pp_block(&mut self, block: Block) -> &mut AstLikePP<'hir> {
        self.pp.str("{...;");
        if let Some(expr) = self.hir.block(block).expr() {
            self.pp_expr(expr);
        }
        self.pp.str("}")
    }

    fn pp_body(&mut self, body: BodyId) -> &mut AstLikePP<'hir> {
        let body = self.hir.body(body);

        self.pp.str("\\");
        body.params.iter().copied().for_each(|pat| {
            self.pp_pat(pat).sp();
        });
        self.pp.str("-> ");

        self.pp_expr(body.value)
    }

    fn pp_pat(&mut self, pat: Pat) -> &mut AstLikePP<'hir> {
        let pat = self.hir.pat(pat);
        match pat.kind() {
            PatKind::Unit => self.pp.str("()"),
            PatKind::Ident(name) => self.pp.string(name),
        }
    }
}
