use std::{collections::HashMap, fmt::Display};

use super::{
    ctx::InferCtx,
    kind::{Kind, KindEx},
    ty::Ex,
    TyResult, TypeckErr,
};
use crate::{
    cli::color::{Color, ColorizedStruct},
    dt::idx::{declare_idx, IndexVec},
    hir::{
        expr::{ExprKind, TyExpr},
        pat::PatKind,
        Block, BodyId, Expr, Pat, HIR,
    },
    interface::writer::{out, Writer},
    pp::{
        pp::{pp, PP},
        AstLikePP, AstPPMode,
    },
    session::Session,
    typeck::ty::Ty,
};

declare_idx!(InferEntryId, u32, "{}", Color::Blue);

pub enum InferEntryKind {
    TryTo,
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
}

pub struct InferStep {
    pub kind: InferStepKind,
}

type Entries = IndexVec<InferEntryId, InferEntry>;
struct IDCtx<'ctx> {
    hir: &'ctx HIR,
    entries: &'ctx Entries,
}

pub struct InferDebug<'ctx> {
    hir: &'ctx HIR,
    pp: PP,
    entries: Entries,
    entry: Option<InferEntryId>,
}

impl<'ctx> AstLikePP<'ctx> {
    fn ty_result<T>(&mut self, res: &TyResult<T>) -> &mut AstLikePP<'ctx>
    where
        T: Display,
    {
        match res {
            Ok(ok) => self.string(format!("Ok({ok})")),
            Err(err) => self.string(match err {
                TypeckErr::Check => "check failed",
                TypeckErr::LateReport => "error (unreported)",
                TypeckErr::Reported => "error (reported)",
            }),
        }
    }

    fn expr(&mut self, expr: Expr, ctx: &IDCtx) -> &mut AstLikePP<'ctx> {
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
        }
    }

    fn block(&mut self, block: Block, ctx: &IDCtx) -> &mut AstLikePP<'ctx> {
        pp!(self, "{...;" "}");

        self.str("{...;");
        if let Some(expr) = ctx.hir.block(block).expr() {
            self.expr(expr, ctx);
        }
        self.str("}")
    }

    fn body(&mut self, body: BodyId, ctx: &IDCtx) -> &mut AstLikePP<'ctx> {
        let body = ctx.hir.body(body);

        self.str("\\");
        body.params.iter().copied().for_each(|pat| {
            self.pat(pat, ctx).sp();
        });

        self.str("-> ").expr(body.value, ctx)
    }

    fn pat(&mut self, pat: Pat, ctx: &IDCtx) -> &mut AstLikePP<'ctx> {
        let pat = ctx.hir.pat(pat);
        match pat.kind() {
            PatKind::Unit => self.str("()"),
            PatKind::Ident(name) => self.string(name),
        }
    }

    fn entry(&mut self, entry: &InferEntry, ctx: &IDCtx) -> &mut AstLikePP<'ctx> {
        self.out_indent();

        match &entry.kind {
            InferEntryKind::TryTo => {
                self.line("Try to");
            },
            &InferEntryKind::ForExpr(expr) => {
                self.str("For expr `").expr(expr, ctx).str("`").nl();
            },
        }

        self.indent();

        entry.steps.iter().for_each(|step| {
            self.step(step, ctx);
        });

        if !entry.children.is_empty() {
            self.line("and then");
        }

        entry.children.iter().copied().for_each(|child| {
            self.entry(ctx.entries.get(child).unwrap(), ctx);
        });

        self.dedent()
    }

    fn step(&mut self, step: &InferStep, ctx: &IDCtx) -> &mut AstLikePP<'ctx> {
        self.out_indent();
        match &step.kind {
            InferStepKind::Synthesized(ty) => self.string(format!("Synthesized {ty}")),
            InferStepKind::CtxApplied(before, after) => {
                self.string(format!("Applying current context on {before} => {after}"))
            },
            InferStepKind::Check(expr, ty, res) => self
                .str("Check ")
                .expr(*expr, ctx)
                .string(format!("is of type {ty}"))
                .ty_result(res),
            InferStepKind::Subtype(ty, subtype_of, res) => self
                .string(format!("{ty} is a subtype of {subtype_of} => "))
                .ty_result(res),
            InferStepKind::SubtypeKind(kind, subkind_of, res) => self
                .string(format!("{kind} is a subkind of {subkind_of} => "))
                .ty_result(res),
            InferStepKind::Solve(ex, ty) => self.string(format!("Solve {ex} = {ty}")),
            InferStepKind::SolveKind(ex, kind) => self.string(format!("Solve {ex} = {kind}")),
        }
    }
}

impl<'ctx> InferDebug<'ctx> {
    pub fn new(hir: &'ctx HIR) -> Self {
        Self {
            hir,
            pp: PP::new(),
            entries: Default::default(),
            entry: None,
        }
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

        self.entry_mut().children.push(id);

        self.entry = Some(id);

        id
    }

    pub fn exit(&mut self, ie: InferEntryId) {
        let mut unwind_to = Some(ie);

        while let Some(id) = unwind_to {
            let entry = self.entries.get(id).unwrap();

            unwind_to = entry.parent;

            if self.entry == unwind_to {
                break;
            }

            self.entries.get_mut(id)
        }

        self.entry = unwind_to;
    }
}
