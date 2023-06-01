use super::{AstLikePP, AstPPMode};
use crate::{
    mir::thir::{BlockId, ExprId, ExprKind, Pat, PatKind, Stmt, StmtId, THIR},
    parser::token::Punct,
    session::Session,
    span::sym::Kw,
};

pub struct ThirPrinter<'a> {
    pp: AstLikePP<'a>,
    thir: &'a THIR,
}

impl<'a> ThirPrinter<'a> {
    pub fn new(sess: &'a Session, thir: &'a THIR) -> Self {
        Self {
            pp: AstLikePP::new(sess, AstPPMode::Normal),
            thir,
        }
    }

    pub fn print(mut self, thir_entry_expr: ExprId) -> String {
        self.expr(thir_entry_expr);
        self.pp.get_string()
    }

    fn stmt(&mut self, id: StmtId) {
        self.pp.out_indent();
        let stmt = self.thir.stmt(id);
        match stmt {
            &Stmt::Expr(id) => self.expr(id),
            Stmt::Local(pat, init) => {
                self.pat(&pat);
                self.expr(*init);
            },
        }
    }

    fn expr(&mut self, id: ExprId) {
        let expr = self.thir.expr(id);
        match &expr.kind {
            ExprKind::Lit(lit) => {
                self.pp.string(lit);
            },
            ExprKind::LocalRef(local) => {
                self.pp.string(local);
            },
            &ExprKind::Def(def_id, ty) => {
                self.pp.color(def_id).punct(Punct::Colon).string(ty);
            },
            &ExprKind::Block(id) => self.block(id),
            &ExprKind::Ref(expr) => {
                self.pp.str("ref ");
                self.expr(expr);
            },
            ExprKind::Call {
                func_ty: _,
                lhs,
                args,
            } => {
                self.expr(*lhs);
                self.pp.punct(Punct::LParen);
                args.iter().copied().for_each(|arg| self.expr(arg));
                self.pp.punct(Punct::RParen);
            },
            &ExprKind::Lambda { def_id, body_id } => {
                self.pp
                    .str("lambda")
                    .color(def_id)
                    .str("{")
                    .string(body_id)
                    .str("}");
            },
            &ExprKind::Ty(expr, ty) => {
                self.expr(expr);
                self.pp.punct(Punct::Colon).string(ty);
            },
            ExprKind::Builtin(bt) => {
                self.pp.string(bt);
            },
            // &ExprKind::FieldAccess(lhs, field, _) => {
            //     self.expr(lhs);
            //     self.pp.punct(Punct::Dot).colorized(field);
            // },
        }
    }

    fn pat(&mut self, pat: &Pat) {
        match pat.kind {
            PatKind::Unit => {
                self.pp.kw(Kw::Unit);
            },
            PatKind::Ident {
                name,
                var: _,
                ty: _,
            } => {
                self.pp.string(name);
            },
        }
    }

    fn block(&mut self, id: BlockId) {
        let block = self.thir.block(id);
        self.pp.indent();
        block.stmts.iter().copied().for_each(|stmt| {
            self.stmt(stmt);
            self.pp.nl();
        });
        self.pp.out_indent();
        block.expr.map(|expr| {
            self.expr(expr);
            self.pp.nl();
        });
        self.pp.dedent();
    }
}

// TODO: Replace THIR building with `thir_built` query when query engine added
// impl<'a> HirVisitor for ThirPrinter<'a> {
//     fn visit_body(&mut self, &body: &BodyId, _owner: BodyOwner) {

//     }
// }
