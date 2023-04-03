use std::{borrow::Borrow, str::from_utf8};

use crate::{
    hir::{
        expr::Lambda,
        item::ItemId,
        pat::{Pat, PatKind},
        visitor::HirVisitor,
        BodyId, BodyOwner, WithHirId, HIR,
    },
    mir::{
        Body, Const, ConstKind, LValue, Operand, RValue, Stmt, StmtKind, Terminator,
        TerminatorKind, MIR,
    },
    session::Session,
    span::span::Ident,
};

use super::{hir::walk_each_delim, AstLikePP};

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
            // RValue::Infix(lhs, op, rhs) => {
            //     self.print_operand(lhs);
            //     self.pp.string(format!(" {} ", op));
            //     self.print_operand(rhs);
            // },
            RValue::Infix(op) => {
                self.pp.string(format!("({})", op));
            },
            RValue::Closure(def_id) => {
                self.pp.string(format!("closure{}", def_id));
            },
            RValue::Call { lhs, args } => {
                self.pp.ch('(');
                self.print_operand(lhs);
                self.pp.sp();
                walk_each_delim!(self, args, print_operand, " ");
                self.pp.ch(')');
                // self.pp.string(format!(" -> {}", target));
            },
            &RValue::FuncRef(def_id, ty) => {
                let def = self.sess.def_table.get_def(def_id);
                // match def.kind() {
                //     DefKind::Func => todo!(),
                //     DefKind::Value => todo!(),
                // }
                self.pp.string(format!("{}: {}", def.name(), ty));
            },
            &RValue::ClosureRef(def_id) | &RValue::ValueRef(def_id) => {
                let def = self.sess.def_table.get_def(def_id);
                self.pp.string(def);
            },
            RValue::Ref(lv) => {
                self.pp.str("ref ");
                self.print_lvalue(lv);
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
            ConstKind::Slice { data } => match const_.ty.kind() {
                crate::typeck::ty::TyKind::Str => {
                    self.pp.string(format!("\"{}\"", from_utf8(data).unwrap()))
                },
                _ => self.pp.string(format!("{:02x?}", data)),
            },
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
        self.visit_body(body, BodyOwner::func(id.def_id()), hir);
    }

    fn visit_lambda(&mut self, lambda: &Lambda, hir: &HIR) {
        self.pp.string(format!("[lambda{}]", lambda.def_id)).sp();
        self.visit_body(&lambda.body_id, BodyOwner::lambda(lambda.def_id), hir);
    }

    fn visit_value_item(&mut self, name: Ident, value: &BodyId, id: ItemId, hir: &HIR) {
        self.pp.string(format!("{} =", name.original_string()));
        self.visit_body(value, BodyOwner::value(id.def_id()), hir);
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

    fn visit_body(&mut self, body: &BodyId, _owner: BodyOwner, hir: &HIR) {
        walk_each_delim!(self, hir.body(*body).params, visit_pat, " ", hir);
        self.pp.sp();

        if let Some(body) = self.mir.bodies.get(body) {
            self.print_body(body);
        } else {
            self.pp.str("[NO BODY]");
        }
    }
}
