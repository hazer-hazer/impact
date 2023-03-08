use crate::{
    hir::{visitor::HirVisitor, BodyId, BodyOwnerKind, OwnerId, HIR},
    mir::{Body, Const, LValue, Operand, RValue, Stmt, StmtKind, Terminator, MIR},
};

use super::AstLikePP;

pub struct MirPrinter<'ctx> {
    pp: AstLikePP<'ctx>,
    hir: &'ctx HIR,
    mir: &'ctx MIR,
}

impl<'ctx> MirPrinter<'ctx> {
    fn print_body(&mut self, body: &Body) {
        for (local, info) in body.locals.iter_enumerated() {
            // TODO: Print additional info such as span
            self.pp.string(format!("{}: {}", local, info.ty)).nl();
        }

        for (bb_id, bb) in body.basic_blocks.iter_enumerated() {
            self.pp.string(format!("bb{}: {{", bb_id)).nl();
            self.pp.indent();
            bb.stmts.iter().for_each(|stmt| {
                self.print_stmt(stmt);
            });
            self.print_terminator(&bb.terminator);
            self.pp.dedent();
            self.pp.ch('}');
        }
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
            RValue::Call { lhs, arg, target } => {
                self.print_operand(lhs);
                self.pp.sp();
                self.print_operand(arg);
                self.pp.string(format!(" -> {}", target));
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
        todo!()
    }

    fn print_terminator(&mut self, terminator: &Terminator) {
        self.pp.out_indent();
    }
}

impl<'ctx> HirVisitor for MirPrinter<'ctx> {
    fn visit_body(&mut self, body: &BodyId, hir: &HIR) {
        let body = self.mir.bodies.get(body).unwrap();
        self.print_body(body);
    }
}
