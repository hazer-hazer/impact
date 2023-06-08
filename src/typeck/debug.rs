use std::{fmt::Display};

use super::{
    kind::{Kind, KindEx},
    ty::{Ex},
    TyResult, TypeckErr,
};
use crate::{
    cli::color::{Color, Colorize, WithColor},
    dt::idx::{declare_idx, IndexVec},
    hir::{
        expr::{Arm, ExprKind},
        pat::PatKind,
        Block, BodyId, Expr, Map, Pat, HIR,
    },
    parser::token::Punct,
    pp::{
        pp::{pp, PP},
    },
    typeck::ty::Ty,
};

declare_idx!(InferEntryId, u32, "{}", Color::Blue);

macro_rules! tcdbg {
    ($self: expr, step $kind: expr) => {
        $self.dbg.step($kind)
    };

    ($self: expr, add $el: expr, $usage: expr) => {{
        use crate::cli::color::{WithColor};
        tcdbg!($self, step InferStepKind::AddCtxEl($el.colorized().to_string(), $usage.to_string()))
    }};

    ($self: expr, add_list $els_iter: expr, $usage: expr) => {{
        use crate::cli::color::{WithColor};
        tcdbg!($self, step InferStepKind::AddCtxElList($els_iter.map(|el| el.colorized().to_string()).collect(), $usage.to_string()))
    }};

    ($self: expr, enter $entry_kind: expr) => {
        $self.dbg.enter($entry_kind)
    };

    ($self: expr, exit $ie: expr) => {
        $self.dbg.exit($ie);
    };

    // ($self: expr, add_kind_var $var: expr, $usage: expr) => {
    //     tcdbg!($self, step InferStepKind::AddKindVar($var, $usage.to_string()))
    // };

    // ($self: expr, add_ex $ex: expr, $usage: expr) => {
    //     tcdbg!($self, step InferStepKind::AddEx($ex, $usage.to_string()))
    // };

    // ($self: expr, add_kind_ex $ex: expr, $usage: expr) => {
    //     tcdbg!($self, step InferStepKind::AddKindEx($ex, $usage.to_string()))
    // };

}

pub(super) use tcdbg;

pub enum InferEntryKind {
    TryTo,
    UnderCtx(usize),
    ForExpr(Expr),
}

pub struct InferEntry {
    id: InferEntryId,
    parent: Option<InferEntryId>,
    kind: InferEntryKind,
    children: Vec<InferEntryId>,
    steps: Vec<InferStep>,
    failed: bool,
}

impl InferEntry {
    pub fn new(id: InferEntryId, parent: Option<InferEntryId>, kind: InferEntryKind) -> Self {
        Self {
            id,
            parent,
            kind,
            children: Default::default(),
            steps: Default::default(),
            failed: false,
        }
    }
}

pub enum InferStepKind {
    /// Result of type synthesis which will be type of the inferred expression.
    Synthesized(Ty),

    /// Before and after context application
    CtxApplied(Ty, Ty),

    /// Check whether expr is of a specific type or not
    Check(Expr, Ty, TyResult<Ty>),

    /// Check if lhs type is subtype of rhs type
    Subtype(Ty, Ty, TyResult<Ty>),

    /// Check if lhs kind is subtype of rhs kind
    SubtypeKind(Kind, Kind, TyResult<Kind>),

    Solve(Ex, Ty),
    SolveKind(KindEx, Kind),

    AddCtxEl(String, String),
    AddCtxElList(Vec<String>, String),
}

pub struct InferStep {
    pub kind: InferStepKind,
}

type Entries = IndexVec<InferEntryId, InferEntry>;
struct IDCtx<'ctx> {
    hir: &'ctx HIR,
    entries: &'ctx Entries,
}

pub struct InferDebug {
    pp: PP,
    entries: Entries,
    entry: Option<InferEntryId>,
}

impl InferDebug {
    pub fn new() -> Self {
        Self {
            pp: PP::new(),
            entries: Default::default(),
            entry: None,
        }
    }

    pub fn get_string(mut self, hir: &HIR) -> String {
        self.pp.entry(
            InferEntryId(0),
            &IDCtx {
                hir,
                entries: &self.entries,
            },
        );
        self.pp.get_string()
    }

    fn entry_mut(&mut self) -> &mut InferEntry {
        self.entries.get_mut(self.entry.unwrap()).unwrap()
    }

    pub fn step(&mut self, kind: InferStepKind) {
        self.entry_mut().steps.push(InferStep { kind })
    }

    pub fn enter(&mut self, kind: InferEntryKind) -> InferEntryId {
        let id = self.entries.len().into();

        self.entries.push(InferEntry::new(id, self.entry, kind));

        if self.entry.is_some() {
            self.entry_mut().children.push(id);
        }

        self.entry = Some(id);

        id
    }

    pub fn exit(&mut self, ie: InferEntryId) {
        let mut unwind_to = Some(ie);

        while let Some(id) = unwind_to {
            let entry = self.entries.get(id).unwrap();

            unwind_to = entry.parent;

            if id == entry.id {
                break;
            }

            self.entries.get_mut(id);
        }

        self.entry = unwind_to;
    }
}

impl PP {
    fn ty_result<T>(&mut self, res: &TyResult<T>) -> &mut PP
    where
        T: Display + WithColor,
    {
        match res {
            Ok(ok) => {
                pp!(self, { "Ok({})", ok.colorized() });
            },
            Err(err) => pp!(self, {str: match err {
                TypeckErr::Check => "check failed",
                TypeckErr::LateReport => "error (unreported)",
                TypeckErr::Reported => "error (reported)",
            }}),
        }
        self
    }

    fn expr(&mut self, expr: Expr, ctx: &IDCtx) -> &mut PP {
        match ctx.hir.expr(expr).kind() {
            ExprKind::Lit(lit) => self.string(lit),
            &ExprKind::Path(path) => self.string(ctx.hir.expr_path(path)),
            &ExprKind::Block(block) => self.block(block, ctx),
            ExprKind::Lambda(lambda) => self.body(lambda.body_id, ctx),
            ExprKind::Call(call) => {
                self.expr(call.lhs, ctx).sp();
                call.args.iter().copied().for_each(|arg| {
                    self.expr(arg, ctx);
                });
                self
            },
            &ExprKind::Let(block) => self.block(block, ctx),
            ExprKind::Ty(ty_expr) => self.expr(ty_expr.expr, ctx).str(": [ty]"),
            &ExprKind::Builtin(bt) => self.string(bt),
            ExprKind::Match(subject, arms) => {
                pp!(self, "match ", {expr: *subject, ctx}, {delim ", " / arm: arms.iter(), ctx}, ...)
            },
        }
    }

    fn arm(&mut self, arm: &Arm, ctx: &IDCtx) -> &mut PP {
        pp!(self, {pat: arm.pat, ctx}, {punct: Punct::FatArrow}, {expr: arm.body, ctx}, ...)
    }

    fn block(&mut self, block: Block, ctx: &IDCtx) -> &mut PP {
        pp!(self, "{...;", {expr?: ctx.hir.block(block).expr(), ctx}, "}", ...)
    }

    fn body(&mut self, body: BodyId, ctx: &IDCtx) -> &mut PP {
        let body = ctx.hir.body(body);

        pp!(
            self,
            "\\",
            {delim " " / pat: body.params.iter().copied(), ctx},
            " -> ",
            {expr: body.value, ctx},
            ...
        )
    }

    fn pat(&mut self, pat: Pat, ctx: &IDCtx) -> &mut PP {
        let pat = ctx.hir.pat(pat);
        match pat.kind() {
            PatKind::Unit => self.str("()"),
            PatKind::Ident(name) => self.string(name),
        }
    }

    fn entry(&mut self, entry: InferEntryId, ctx: &IDCtx) -> &mut PP {
        let entry = ctx.entries.get(entry).unwrap();

        pp!(self, { out_indent }, {if (entry.failed) "❌"}, {str: "> "});

        match &entry.kind {
            InferEntryKind::TryTo => {
                pp!(self, {line: "Try to"});
            },
            &InferEntryKind::ForExpr(expr) => {
                pp!(self, "For expr `", {expr: expr, ctx}, "`", {nl});
            },
            InferEntryKind::UnderCtx(depth) => {
                pp!(
                    self,
                    { "Under new context [depth: {}]", depth },
                    { nl }
                );
            },
        }

        pp!(
            self,
            {indent},
            {delim {nl} / step: entry.steps.iter(), ctx},
            {if (entry.steps.is_empty()) [out_indent, "[Nothing done]"]},
            {nl, out_indent, indent},
            {if (!entry.children.is_empty()) {line: "and then"} else {line: "[No subentries]"}},
            {delim {nl} / entry: entry.children.iter().copied(), ctx},
            {dedent, dedent},
            ...
        )
    }

    fn step(&mut self, step: &InferStep, ctx: &IDCtx) -> &mut PP {
        self.out_indent();
        match &step.kind {
            &InferStepKind::Synthesized(ty) => pp!(self, "Synthesized ", { color: ty }),
            &InferStepKind::CtxApplied(before, after) => {
                pp!(
                    self,
                    "Applying current context on ",
                    { color: before },
                    " => ",
                    { color: after },
                    {if (before == after) {string: " (No difference)".yellow()}}
                );
            },
            &InferStepKind::Check(expr, ty, ref res) => {
                pp!(self, "Check ", {expr: expr, ctx}, " is of type ", {color: ty}, " => ", {ty_result: res});
            },
            &InferStepKind::Subtype(ty, subtype_of, ref res) => {
                pp!(
                    self,
                    { color: ty },
                    " is a subtype of ",
                    { color: subtype_of },
                    " => ",
                    { ty_result: res }
                );
            },
            &InferStepKind::SubtypeKind(kind, subkind_of, ref res) => {
                pp!(
                    self,
                    { color: kind },
                    " is a subkind of ",
                    { color: subkind_of },
                    " => ",
                    { ty_result: res }
                );
            },
            &InferStepKind::Solve(ex, ty) => {
                pp!(self, "Solve ", { color: ex }, " = ", { color: ty });
            },
            &InferStepKind::SolveKind(ex, kind) => {
                pp!(self, "Solve ", { color: ex }, " = ", { color: kind });
            },
            InferStepKind::AddCtxEl(el, usage) => {
                pp!(self, "Add ", { string: el }, " for ", { string: usage });
            },
            InferStepKind::AddCtxElList(els, usage) => {
                pp!(self, "Add [", { delim {", "} / string: els.iter() }, "] for ", { string: usage })
            },
        }

        self
    }
}
