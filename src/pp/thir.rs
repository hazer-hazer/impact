use inkwell::values;

use super::{
    pp::{pp, PP},
    AstLikePP, AstPPMode,
};
use crate::{
    hir::BodyId,
    mir::thir::{Arm, BlockId, ExprId, ExprKind, Pat, PatId, PatKind, Stmt, StmtId, THIR},
    parser::token::{Op, Punct},
    session::Session,
    span::sym::Kw,
};

pub struct ThirPPCtx<'a> {
    thir: &'a THIR,
}

impl<'a> ThirPPCtx<'a> {
    pub fn new(thir: &'a THIR) -> Self {
        Self { thir }
    }
}

pub trait ThirPrinter {
    fn pp(self, thir_entry_expr: ExprId) -> Self;
    fn stmt(&mut self, id: StmtId) -> &mut Self;
    fn expr(&mut self, id: ExprId) -> &mut Self;
    fn arm(&mut self, arm: &Arm) -> &mut Self;
    fn pat(&mut self, pat: PatId) -> &mut Self;
    fn block(&mut self, id: BlockId) -> &mut Self;
}

impl<'a> ThirPrinter for PP<ThirPPCtx<'a>> {
    fn pp(mut self, thir_entry_expr: ExprId) -> Self {
        pp!(self, {expr: thir_entry_expr}, ...)
    }

    fn stmt(&mut self, id: StmtId) -> &mut Self {
        pp!(self, { out_indent });
        let stmt = self.ctx().thir.stmt(id);
        match stmt {
            &Stmt::Expr(id) => self.expr(id),
            &Stmt::Local(pat, init) => {
                self.pat(pat);
                self.expr(init)
            },
        }
    }

    fn expr(&mut self, id: ExprId) -> &mut Self {
        let expr = self.ctx().thir.expr(id);
        match &expr.kind {
            ExprKind::Lit(lit) => {
                pp!(self, { "{lit}" }, ...)
            },
            ExprKind::LocalRef(local) => {
                pp!(self, { "{local}" }, ...)
            },
            &ExprKind::Def(def_id, kind, ty) => {
                pp!(self, {string: kind}, {sp}, {color: def_id}, {punct: Punct::Colon}, {string: ty}, ...)
            },
            &ExprKind::Block(id) => self.block(id),
            &ExprKind::Ref(expr) => {
                pp!(self, "ref", {sp}, {expr: expr}, ...)
            },
            ExprKind::Call {
                func_ty: _,
                lhs,
                args,
            } => {
                pp!(self, {expr: *lhs}, {punct: Punct::LParen}, {delim {sp} / expr: args.iter().copied()}, {punct: Punct::RParen}, ...)
            },
            ExprKind::Tuple(values) => {
                pp!(self, {punct: Punct::LParen}, {delim {punct: Punct::Comma} / expr: values.iter().copied()}, {punct: Punct::RParen}, ...)
            },
            &ExprKind::Lambda { def_id, body_id } => {
                pp!(self, "lambda", {color: def_id}, "{", {string: body_id}, "}", ...)
            },
            &ExprKind::Ty(expr, ty) => {
                pp!(self, {expr: expr}, {punct: Punct::Colon}, {string: ty}, ...)
            },
            ExprKind::Builtin(bt) => {
                pp!(self, {string: bt}, ...)
            },
            ExprKind::Match(subject, arms) => {
                pp!(self, "match", {sp}, {expr: *subject}, {delim {nl} / arm: arms.iter()}, ...)
            },
            // &ExprKind::FieldAccess(lhs, field, _) => {
            //     self.expr(lhs);
            //     self.pp.punct(Punct::Dot).colorized(field);
            // },
        }
    }

    fn arm(&mut self, arm: &Arm) -> &mut Self {
        pp!(self, {pat: arm.pat}, {punct: Punct::FatArrow}, {expr: arm.body}, ...)
    }

    fn pat(&mut self, pat: PatId) -> &mut Self {
        match &self.ctx().thir.pat(pat).kind {
            PatKind::Unit => {
                pp!(self, {kw: Kw::Unit}, ...)
            },
            PatKind::Ident {
                name,
                var: _,
                ty: _,
            } => {
                pp!(self, {string: name}, ...)
            },
            &PatKind::Or(lpat, rpat) => pp!(self, {pat: lpat}, {op: Op::BitOr}, {pat: rpat}, ...),
            PatKind::Struct(..) => todo!(),
            PatKind::Tuple(pats) => {
                pp!(self, {punct: Punct::LParen}, {delim {punct: Punct::Comma} / pat: pats.iter().copied()}, {punct: Punct::RParen}, ...)
            },
        }
    }

    fn block(&mut self, id: BlockId) -> &mut Self {
        let block = self.ctx().thir.block(id);
        pp!(self, {indent}, {delim {nl, out_indent} / stmt: block.stmts.iter().copied()}, {expr?: block.expr}, {nl, dedent}, ...)
    }
}

// TODO: Replace THIR building with `thir_built` query when query engine added
// impl<'a> HirVisitor for ThirPrinter<'a> {
//     fn visit_body(&mut self, &body: &BodyId, _owner: BodyOwner) {

//     }
// }
