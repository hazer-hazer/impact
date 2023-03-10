use crate::{
    hir::{
        expr::Lambda,
        item::ItemId,
        pat::{Pat, PatKind},
        visitor::HirVisitor,
        BodyId, BodyOwnerKind, OwnerId, WithHirId, HIR,
    },
    mir::{
        Body, Const, ConstKind, LValue, Operand, RValue, Stmt, StmtKind, Terminator,
        TerminatorKind, MIR,
    },
    parser::token::Punct,
    resolve::def::DefKind,
    session::Session,
    span::span::Ident,
};

use super::AstLikePP;

pub struct MirPrinter<'ctx> {
    pub pp: AstLikePP<'ctx>,
    sess: &'ctx Session,
    mir: &'ctx MIR,
}

impl<'ctx> MirPrinter<'ctx> {
    pub fn new(sess: &'ctx Session, mir: &'ctx MIR) -> Self {
        Self {
            pp: AstLikePP::new(&sess, super::AstPPMode::Normal),
            sess,
            mir,
        }
    }

    fn print_body(&mut self, body: &Body) {
        self.pp.ch('{').nl().indent();
        for (local, info) in body.locals.iter_enumerated() {
            // TODO: Print additional info such as span
            self.pp
                .out_indent()
                .string(format!("{}: {}", local, info.ty))
                .nl();
        }

        for (bb_id, bb) in body.basic_blocks.iter_enumerated() {
            self.pp
                .out_indent()
                .string(format!("{}: {{", bb_id))
                .nl()
                .indent();
            bb.stmts.iter().for_each(|stmt| {
                self.print_stmt(stmt);
            });
            self.print_terminator(&bb.terminator);
            self.pp.nl().dedent().out_indent().ch('}').nl();
        }
        self.pp.dedent().out_indent().ch('}').nl();
    }

    fn print_stmt(&mut self, stmt: &Stmt) {
        self.pp.out_indent();
        match &stmt.kind {
            StmtKind::Assign(lvalue, rvalue) => {
                self.print_lvalue(lvalue);
                self.pp.str(" = ");
                self.print_rvalue(rvalue);
            },
        }
        self.pp.ch(';').nl();
    }

    fn print_lvalue(&mut self, lvalue: &LValue) {
        self.pp.string(lvalue.local);
    }

    fn print_rvalue(&mut self, rvalue: &RValue) {
        match rvalue {
            RValue::Operand(operand) => self.print_operand(operand),
            RValue::Infix(lhs, op, rhs) => {
                self.print_operand(lhs);
                self.pp.string(format!(" {} ", op));
                self.print_operand(rhs);
            },
            RValue::Closure(def_id) => {
                self.pp.string(format!("closure{}", def_id));
            },
            RValue::Call { lhs, arg } => {
                self.pp.ch('(');
                self.print_operand(lhs);
                self.pp.sp();
                self.print_operand(arg);
                self.pp.ch(')');
                // self.pp.string(format!(" -> {}", target));
            },
            &RValue::Def(def_id, ty) => {
                let def = self.sess.def_table.get_def(def_id);
                // match def.kind() {
                //     DefKind::Func => todo!(),
                //     DefKind::Value => todo!(),
                // }
                self.pp.string(format!("{}: {}", def.name(), ty));
            },
        }
    }

    fn print_operand(&mut self, operand: &Operand) {
        match operand {
            Operand::LValue(lvalue) => self.print_lvalue(lvalue),
            Operand::Const(const_) => self.print_const(const_),
        }
    }

    fn print_const(&mut self, const_: &Const) {
        match &const_.kind {
            ConstKind::Scalar(scalar) => self.pp.string(scalar),
            // FIXME: ZeroSized formatting?
            ConstKind::ZeroSized => self.pp.str("()"),
        };
        self.pp.string(format!(": {}", const_.ty));
    }

    fn print_terminator(&mut self, terminator: &Terminator) {
        self.pp.out_indent();
        match &terminator.kind {
            TerminatorKind::Goto(target) => self.pp.string(format!("goto {}", target)),
            TerminatorKind::Return => self.pp.str("return"),
        };
    }
}

impl<'ctx> HirVisitor for MirPrinter<'ctx> {
    fn visit_func_item(&mut self, name: Ident, body: &BodyId, id: ItemId, hir: &HIR) {
        if id.def_id() == self.sess.def_table.builtin_func().def_id() {
            return;
        }

        self.pp.str("func").sp().string(name.original_string()).sp();
        self.visit_body(body, hir);
    }

    fn visit_lambda(&mut self, lambda: &Lambda, hir: &HIR) {
        self.pp.string(format!("[lambda{}]", lambda.def_id)).sp();
        self.visit_body(&lambda.body_id, hir);
    }

    // Note: Only used for parameters
    fn visit_pat(&mut self, &pat: &Pat, hir: &HIR) {
        let pat = hir.pat(pat);
        match pat.kind() {
            PatKind::Unit => self.pp.str("()"),
            PatKind::Ident(ident) => {
                self.pp
                    .string(format!("{}: {}", ident, self.sess.tyctx.tyof(pat.id())))
            },
        };
    }

    fn visit_body(&mut self, body: &BodyId, hir: &HIR) {
        self.visit_pat(&hir.body(*body).param, hir);
        self.pp.sp();

        let body = self.mir.bodies.get(body).unwrap();
        self.print_body(body);
    }
}
