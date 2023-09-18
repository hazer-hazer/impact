use std::str::from_utf8;

use inkwell::values;

use super::{
    hir::walk_each_delim,
    pp::{pp, PP},
    AstLikePP,
};
use crate::{
    hir::{
        expr::Lambda, item::ItemId, pat::PatKind, visitor::HirVisitor, BodyId, BodyOwner, Map, Pat,
        WithHirId, HIR,
    },
    mir::{
        Body, Const, ConstKind, LValue, Local, LocalInfo, Operand, ProjectionKind, RValue, Stmt,
        StmtKind, Terminator, TerminatorKind, MIR,
    },
    parser::token::{Op, Punct},
    session::{impl_session_holder, Session, SessionHolder},
    span::sym::{Ident, Kw},
};

pub struct MirPPCtx<'ctx> {
    hir: &'ctx HIR,
    sess: &'ctx Session,
    mir: &'ctx MIR,
}

impl<'ctx> MirPPCtx<'ctx> {
    pub fn new(hir: &'ctx HIR, sess: &'ctx Session, mir: &'ctx MIR) -> Self {
        Self { hir, sess, mir }
    }
}

impl_session_holder!(MirPPCtx<'ctx>);

pub trait MirPrinter {
    // fn print_body_local(&mut self, local: (Local, &LocalInfo)) -> &mut Self;
    fn print_body(&mut self, body: &Body) -> &mut Self;
    fn print_stmt(&mut self, stmt: &Stmt) -> &mut Self;
    fn lvalue(&mut self, lvalue: &LValue) -> &mut Self;
    fn rvalue(&mut self, rvalue: &RValue) -> &mut Self;
    fn operand(&mut self, operand: &Operand) -> &mut Self;
    fn const_(&mut self, const_: &Const) -> &mut Self;
    fn terminator(&mut self, terminator: &Terminator) -> &mut Self;
}

impl<'ctx> MirPrinter for PP<MirPPCtx<'ctx>> {
    // fn print_body_local(&mut self, (local, info): (Local, &LocalInfo)) -> &mut
    // Self {     pp!(self, {out_indent}, {"{}: {} // `{}`", local, info.ty,
    // info.name}, ...)
    // }

    fn print_body(&mut self, body: &Body) -> &mut Self {
        pp!(
            self,
            "{", {nl, indent},
        );

        for (local, info) in body.locals.iter_enumerated() {
            // TODO: Print additional info such as span
            pp!(self, {out_indent}, {"{}: {} // `{}`", local, info.ty, info.name}, {nl});
        }

        for (bb_id, bb) in body.basic_blocks.iter_enumerated() {
            pp!(self, {out_indent}, {"{}: {{", bb_id}, {nl, indent}, {each / print_stmt: bb.stmts.iter()});
            self.terminator(&bb.terminator);
            pp!(self, {nl, dedent, out_indent}, "}", {nl});
        }
        pp!(self, {dedent, out_indent}, "}", {nl}, ...)
    }

    fn print_stmt(&mut self, stmt: &Stmt) -> &mut Self {
        pp!(self, { out_indent });
        match &stmt.kind {
            StmtKind::Assign(lvalue, rvalue) => {
                pp!(self, { lvalue: lvalue }, { op: Op::Assign }, {
                    rvalue: rvalue
                });
            },
        }
        pp!(self, {punct: Punct::Semi}, {nl}, ...)
    }

    fn lvalue(&mut self, lvalue: &LValue) -> &mut Self {
        pp!(self, {string: lvalue.local});

        if let Some(proj) = lvalue.proj {
            match proj.kind {
                ProjectionKind::Field(vid, fid) => {
                    pp!(self, {string: vid}, {punct: Punct::Dot}, {string: fid}, ...)
                },
            }
        } else {
            self
        }
    }

    fn rvalue(&mut self, rvalue: &RValue) -> &mut Self {
        match rvalue {
            RValue::Operand(operand) => self.operand(operand),
            // RValue::Infix(lhs, op, rhs) => {
            //     self.print_operand(lhs);
            //     self.pp.string(format!(" {} ", op));
            //     self.print_operand(rhs);
            // },
            RValue::Infix(op) => {
                pp!(self, {punct: Punct::LParen}, {string: op}, {punct: Punct::RParen}, ...)
            },
            RValue::Closure(def_id) => {
                pp!(self, {"closure{}", def_id}, ...)
            },
            RValue::Call { lhs, args } => {
                pp!(self, "(", {operand: lhs}, ")", {sp}, {delim " " / operand: args.iter()}, ...)
                // self.pp.string(format!(" -> {}", target));
            },
            RValue::Tuple(values) => {
                pp!(self, {punct: Punct::LParen}, {delim ", " / operand: values.iter()}, {punct: Punct::RParen}, ...)
            },
            &RValue::FuncRef(def_id, ty) => {
                let def = self.ctx().sess.def_table.def(def_id);
                // match def.kind() {
                //     DefKind::Func => todo!(),
                //     DefKind::Value => todo!(),
                // }
                pp!(self, {"{}: {}", def.name(), ty}, ...)
            },
            &RValue::ClosureRef(def_id) | &RValue::ValueRef(def_id) => {
                let def = self.ctx().sess.def_table.def(def_id);
                pp!(self, {string: def}, ...)
            },
            RValue::Ref(lv) => {
                pp!(self, "ref ", {lvalue: lv}, ...)
            },
            RValue::Ctor(def_id, ty) => {
                pp!(self, {"ctor{def_id}: {ty}"}, ...)
            },
            RValue::FieldAccessor(def_id, ty) => {
                pp!(self, {"field_accessor{def_id}: {ty}"}, ...)
            },
        }
    }

    fn operand(&mut self, operand: &Operand) -> &mut Self {
        match operand {
            Operand::LValue(lvalue) => self.lvalue(lvalue),
            Operand::Const(const_) => self.const_(const_),
        }
    }

    fn const_(&mut self, const_: &Const) -> &mut Self {
        match &const_.kind {
            ConstKind::Scalar(scalar) => pp!(self, { string: scalar }),
            // FIXME: ZeroSized formatting?
            ConstKind::ZeroSized => pp!(self, { kw: Kw::Unit }),
            ConstKind::Slice { data } => match const_.ty.kind() {
                crate::typeck::ty::TyKind::Str => {
                    pp!(self, {"\"{}\"", from_utf8(data).unwrap()});
                },
                _ => {
                    pp!(self, {"{:02x?}", data});
                },
            },
        };
        pp!(self, {punct: Punct::Colon}, {string: const_.ty}, ...)
    }

    fn terminator(&mut self, terminator: &Terminator) -> &mut Self {
        pp!(self, { out_indent });
        match &terminator.kind {
            TerminatorKind::Goto(target) => pp!(self, {"goto {}", target}, ...),
            TerminatorKind::Return => pp!(self, "return", ...),
            TerminatorKind::Switch(operand, targets) => {
                pp!(self, { "switch {operand} {targets}" }, ...)
            },
        }
    }
}

impl<'ctx> HirVisitor for PP<MirPPCtx<'ctx>> {
    fn visit_func_item(&mut self, name: Ident, body: BodyId, id: ItemId, hir: &HIR) {
        if id.def_id() == self.ctx().sess.def_table.builtin_func().def_id() {
            return;
        }

        pp!(self, "func", {sp}, {string: name.original_string()}, {sp});
        self.visit_body(body, BodyOwner::func(id.def_id()), hir);
    }

    fn visit_lambda(&mut self, lambda: &Lambda, hir: &HIR) {
        pp!(self, "[lambda", {string: lambda.def_id}, "]");
        self.visit_body(lambda.body_id, BodyOwner::lambda(lambda.def_id), hir);
    }

    fn visit_value_item(&mut self, name: Ident, value: BodyId, id: ItemId, hir: &HIR) {
        pp!(self, {string: name.original_string()}, {op: Op::Assign});
        self.visit_body(value, BodyOwner::value(id.def_id()), hir);
    }

    // Note: Only used for parameters
    fn visit_pat(&mut self, pat: Pat, hir: &HIR) {
        let pat = self.ctx().hir.pat(pat);
        match pat.kind() {
            PatKind::Unit => pp!(self, {kw: Kw::Unit}, ...),
            &PatKind::Ident(ident, name_id) => {
                pp!(self, {string: ident}, {punct: Punct::Colon}, {string: self.ctx().sess.tyctx().tyof(name_id.id())}, ...)
            },
            PatKind::Struct(..) => todo!(),
            &PatKind::Or(lpat, rpat) => {
                pp!(self, {visit_pat: lpat, hir}, {op: Op::BitOr}, {visit_pat: rpat, hir}, ...)
            },
            PatKind::Tuple(pats) => {
                pp!(self, {punct: Punct::LParen}, {delim ", " / visit_pat: pats.iter().copied(), hir}, {punct: Punct::RParen}, ...)
            },
        };
    }

    fn visit_body(&mut self, body: BodyId, _owner: BodyOwner, hir: &HIR) {
        pp!(self, {delim {sp} / visit_param: self.ctx().hir.body(body).params.iter().copied(), hir}, {sp});

        if let Some(body) = self.ctx().mir.bodies.get(&body) {
            self.print_body(body);
        } else {
            pp!(self, "[NO BODY]", { nl });
        }
    }
}
