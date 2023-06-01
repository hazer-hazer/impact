use std::fmt::Display;

use self::{
    ctx::{AlgoCtx, GlobalCtx, InferCtx},
    debug::InferDebug,
    kind::{Kind, KindEx, KindSort, KindVarId, MonoKind},
    ty::{Ex, ExKind, ExPair, Ty, TyKind, TyVarId},
    ty_infer::MonoTy,
    tyctx::TyCtx,
};
use crate::{
    cli::verbose,
    hir::{HirId, HIR},
    interface::writer::outln,
    message::message::{impl_message_holder, MessageHolder, MessageStorage},
    resolve::def::DefId,
    session::{impl_session_holder, stage_result, Session, SessionHolder, Stage, StageResult},
    typeck::{
        conv::TyConv,
        debug::{tcdbg, InferEntryKind, InferStepKind},
        kind::MonoKindSort,
        ty_infer::MonoTyKind,
    },
};

pub mod builtin;
mod check;
mod conv;
mod ctx;
mod debug;
mod err;
pub mod kind;
mod synth;
pub mod ty;
mod ty_infer;
pub mod tyctx;

#[derive(Debug, Clone, Copy)]
pub enum TypeckErr {
    // TypeckErr used as condition
    Check,

    // Error must be reported somewhere
    LateReport,

    Reported,
}

impl TypeckErr {
    pub fn assert_reported(&self) {
        match self {
            TypeckErr::Reported => {},
            TypeckErr::Check | TypeckErr::LateReport => panic!(),
        }
    }
}

pub struct Typed<T>(T, Ty);

impl<T> Typed<T> {
    pub fn new(node: T, ty: Ty) -> Self {
        Self(node, ty)
    }

    pub fn node(&self) -> &T {
        &self.0
    }

    pub fn ty(&self) -> Ty {
        self.1
    }
}

impl<T> Display for Typed<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

pub type TyResult<T> = Result<T, TypeckErr>;

pub struct Typecker<'hir> {
    // Typecker context //
    global_ctx: GlobalCtx,
    ctx_stack: Vec<InferCtx>,
    try_mode: bool,

    //
    msg: MessageStorage,
    sess: Session,

    // Debug //
    dbg: InferDebug<'hir>,
}

impl_message_holder!(Typecker<'hir>);
impl_session_holder!(Typecker<'hir>);

impl<'hir> Typecker<'hir> {
    pub fn new(sess: Session) -> Self {
        Self {
            global_ctx: Default::default(),
            ctx_stack: Default::default(),
            try_mode: false,

            sess,
            msg: Default::default(),

            dbg: InferDebug::new(sess.hir()),
        }
    }

    fn tyctx(&self) -> &TyCtx {
        &self.sess.tyctx
    }

    fn tyctx_mut(&mut self) -> &mut TyCtx {
        &mut self.sess.tyctx
    }

    // Errors //
    fn ty_illformed(&mut self, ty: Ty) -> TyResult<Ty> {
        panic!("Illformed type {}; Context:\n{}", ty, self.dump_ctx_stack());
        // Err(TypeckErr::Check)
    }

    fn kind_illformed(&mut self, kind: Kind) -> TyResult<Kind> {
        panic!(
            "Illformed kind {}; Context:\n{}",
            kind,
            self.dump_ctx_stack()
        );
    }

    fn type_inferring_node<Id>(&mut self, id: Id, ty: Ty)
    where
        Id: Into<HirId> + Copy,
    {
        self.tyctx_mut().type_node(id, ty);
        // self.ctx_mut().add_inferring_node(id.into());
    }

    // fn cyclic_ty(&mut self, ty: Ty, references: Ty, on: ExistentialId) {
    //     MessageBuilder::error()
    //     .text(format!("Cyclic type {}", self.tyctx().ty(ty)))
    //     .label(span, text)
    // }

    // Context //
    fn ctx(&self) -> &InferCtx {
        self.ctx_stack.last().unwrap()
    }

    fn ctx_mut(&mut self) -> &mut InferCtx {
        self.ctx_stack.last_mut().unwrap()
    }

    fn ctx_depth(&self) -> usize {
        self.ctx_stack.len()
    }

    // fn verify_ctx(&mut self) {
    //     self.ctx()
    //         .inferring_nodes()
    //         .iter()
    //         .copied()
    //         .map(|id| self.must_be_inferred(id))
    //         .filter_map(|msg| msg)
    //         .collect::<Vec<_>>()
    //         .into_iter()
    //         .for_each(|msg| {
    //             msg.emit(self);
    //         });
    // }

    fn enter_ctx(&mut self, ctx: InferCtx) {
        self.ctx_stack.push(ctx);
    }

    fn exit_ctx(&mut self) {
        // assert!(self.ctx().unsolved().is_empty());
        verbose!("< [EXIT CTX {}]", self.ctx_depth());

        let depth = self.ctx_depth();

        self.global_ctx.add(depth, self.ctx_stack.pop().unwrap());
    }

    /// Returns try mode flag and context depth before try mode to restore to
    /// them
    fn enter_try_mode(&mut self) -> (bool, usize) {
        let restore = (self.try_mode, self.ctx_depth());

        verbose!("?> [ENTER TRY CTX {}]", self.ctx_depth() + 1);

        self.enter_ctx(InferCtx::default());
        self.try_mode = true;
        restore
    }

    fn exit_try_mode<T>(
        &mut self,
        res: TyResult<T>,
        (before_try_mode, try_depth): (bool, usize),
    ) -> TyResult<T> {
        self.try_mode = before_try_mode;

        match res {
            Ok(ok) => Ok(ok),
            Err(err) => {
                self.ctx_stack.truncate(try_depth);
                Err(err)
            },
        }
    }

    pub fn under_ctx<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        verbose!("> [ENTER CTX {}]", self.ctx_depth() + 1);

        let ie = tcdbg!(self, enter InferEntryKind::UnderCtx(self.ctx_depth()));

        self.enter_ctx(InferCtx::default());
        let res = f(self);
        self.exit_ctx();

        tcdbg!(self, exit ie);

        res
    }

    fn try_to<T>(&mut self, mut f: impl FnMut(&mut Self) -> TyResult<T>) -> TyResult<T>
    where
        T: Display + Copy,
    {
        let restore = self.enter_try_mode();
        let ie = tcdbg!(self, enter InferEntryKind::TryTo);
        let res = f(self);
        tcdbg!(self, exit ie);
        self.exit_try_mode(res, restore)
    }

    /// Goes up from current context to the root looking for something in each
    /// scope Returns result of the callback (if some returned) and depth of
    /// context scope where callback returned result
    fn _ascend_ctx<T>(&self, mut f: impl FnMut(&InferCtx) -> Option<T>) -> Option<(T, usize)> {
        let mut ctx_depth = self.ctx_depth() - 1;

        loop {
            let res = f(&self.ctx_stack[ctx_depth]);
            if let Some(res) = res {
                return Some((res, ctx_depth + 1));
            }

            if ctx_depth == 0 {
                break;
            }

            ctx_depth -= 1;
        }

        None
    }

    fn _ascend_ctx_mut<T>(
        &mut self,
        mut f: impl FnMut(&mut InferCtx) -> Option<T>,
    ) -> Option<(T, usize)> {
        let mut ctx_depth = self.ctx_depth() - 1;

        loop {
            let res = f(&mut self.ctx_stack[ctx_depth]);
            if let Some(res) = res {
                return Some((res, ctx_depth + 1));
            }

            if ctx_depth == 0 {
                break;
            }

            ctx_depth -= 1;
        }

        None
    }

    fn ascend_ctx<T>(&self, f: impl FnMut(&InferCtx) -> Option<T>) -> Option<T> {
        self._ascend_ctx(f).map_or(None, |(val, _depth)| Some(val))
    }

    /// Returns (existential context depth, existential position in context)
    fn find_unbound_ex_depth(&self, ex: Ex) -> (usize, usize) {
        self._ascend_ctx(|ctx| ctx.get_ex_index(ex))
            .or_else(|| self.global_ctx.get_ex_index(ex))
            .expect(&format!(
                "Undefined existential {}; ctx {}",
                ex,
                self.dump_ctx_stack()
            ))
    }

    fn find_unbound_kind_ex_depth(&self, ex: KindEx) -> (usize, usize) {
        self._ascend_ctx(|ctx| ctx.get_kind_ex_index(ex))
            .or_else(|| self.global_ctx.get_kind_ex_index(ex))
            .expect(&format!(
                "Undefined existential {}; ctx {}",
                ex,
                self.dump_ctx_stack()
            ))
    }

    fn fresh_ex(&mut self, sort: ExKind) -> Ex {
        Ex::new(sort, self.tyctx_mut().fresh_ex())
    }

    fn fresh_kind_ex(&mut self) -> KindEx {
        KindEx::new(self.tyctx_mut().fresh_kind_ex())
    }

    fn add_fresh_ex(&mut self, sort: ExKind) -> ExPair {
        let ex = self.fresh_ex(sort);
        self.add_ex(ex);
        (ex, Ty::ex(ex))
    }

    fn add_fresh_common_ex(&mut self) -> ExPair {
        self.add_fresh_ex(ExKind::Common)
    }

    fn add_fresh_kind_ex(&mut self) -> (KindEx, Kind) {
        let kind_ex = self.fresh_kind_ex();
        self.add_kind_ex(kind_ex);
        (kind_ex, Kind::new_ex(kind_ex))
    }

    fn add_fresh_kind_ex_as_ty(&mut self) -> ExPair<KindEx> {
        let (ex, kind) = self.add_fresh_kind_ex();
        (ex, Ty::ty_kind(kind))
    }

    fn is_unsolved_ex(&self, ty: Ty) -> Option<Ex> {
        if let Some(ex) = ty.as_ex() {
            if let None = self.get_solution(ex) {
                return Some(ex);
            }
        }
        None
    }

    fn is_unsolved_kind_ex(&self, ty: Ty) -> Option<KindEx> {
        if let Some(ex) = ty.as_kind_ex() {
            if let None = self.get_kind_ex_solution(ex) {
                return Some(ex);
            }
        }
        None
    }

    fn add_fresh_common_ex_list(&mut self, count: usize) -> Vec<ExPair> {
        (0..count)
            .into_iter()
            .map(|_| self.add_fresh_common_ex())
            .collect()
    }

    // // TODO: Solutions must be stored just in the current context, DO NOT ASCEND
    fn solve(&mut self, ex: Ex, sol: MonoTy) -> Ty {
        tcdbg!(self, step InferStepKind::Solve(ex, sol.ty));
        if let &MonoTyKind::Ex(sol_ex) = &sol.sort {
            assert_ne!(
                sol_ex, ex,
                "Tried to solve ex with itself {} / {}",
                ex, sol.ty
            );
        }
        let ty = sol.ty;
        verbose!("Solve {} as {} in [CTX {}]", ex, ty, self.ctx_depth());
        self.ctx_mut().solve(ex, sol);
        ty
        // self._ascend_ctx_mut(|ctx| ctx.solve(ex, sol))
        //     .map_or_else(|| self.global_ctx.solve(ex, sol), |(ty, _)|
        // Some(ty))     .unwrap()
    }

    fn solve_kind_ex(&mut self, ex: KindEx, sol: MonoKind) -> Kind {
        tcdbg!(self, step InferStepKind::SolveKind(ex, sol.kind));
        if let MonoKindSort::Ex(sol_ex) = sol.sort {
            assert_ne!(
                sol_ex, ex,
                "Tried to solve kind ex with itself {} / {}",
                ex, sol
            );
        }

        let kind = sol.kind;
        verbose!("Solve {} as {} in [CTX {}]", ex, kind, self.ctx_depth());
        self.ctx_mut().solve_kind_ex(ex, sol);
        kind
    }

    fn add_ex(&mut self, ex: Ex) {
        verbose!("Add ex {ex}");
        self.ctx_mut().add_ex(ex);
    }

    fn add_kind_ex(&mut self, kind_ex: KindEx) {
        self.ctx_mut().add_kind_ex(kind_ex);
    }

    fn add_var(&mut self, var: TyVarId) -> Ty {
        verbose!("Add var {var}");
        self.ctx_mut().add_var(var);
        Ty::var(var)
    }

    fn add_kind_var(&mut self, var: KindVarId) -> Kind {
        verbose!("Add kind var {var}");
        self.ctx_mut().add_kind_var(var);
        Kind::new_var(var)
    }

    fn is_unsolved(&self, ex: Ex) -> bool {
        self.get_solution(ex).is_none()
    }

    // We cannot verify type well-formedness as we already deviate from CaEBTC
    // pub fn ty_wf(&mut self, ty: Ty) -> TyResult<Ty> {
    //     match ty.kind() {
    //         TyKind::Error => Ok(ty),
    //         TyKind::Unit | TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) |
    // TyKind::Str => Ok(ty),         &TyKind::Var(_ident) => {
    //             // FIXME
    //             Ok(ty)
    //             // if self.ascend_ctx(|ctx| ctx.get_var(ident)).is_some() {
    //             //     Ok(ty)
    //             // } else {
    //             //     self.ty_illformed(ty)
    //             // }
    //         },
    //         &TyKind::Existential(ex) => {
    //             if self.ascend_ctx(|ctx| ctx.get_ex(ex)).is_some() ||
    // self.global_ctx.has_ex(ex) {                 Ok(ty)
    //             } else {
    //                 self.ty_illformed(ty)
    //             }
    //         },
    //         TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
    //             params.iter().copied().try_for_each(|param| {
    //                 self.ty_wf(param)?;
    //                 Ok(())
    //             })?;
    //             self.ty_wf(*body)
    //         },
    //         TyKind::Adt(adt) => {
    //             adt.walk_tys().try_for_each(|ty| {
    //                 self.ty_wf(ty)?;
    //                 Ok(())
    //             })?;
    //             Ok(ty)
    //         },
    //         &TyKind::Forall(alpha, body) =>
    // self.under_ctx(InferCtx::new_with_var(alpha), |this| {             // let
    // open_forall = this.open_forall(body, alpha);             this.ty_wf(body)
    //         }),
    //         &TyKind::Ref(inner) => self.ty_wf(inner),
    //         &TyKind::Kind(kind) => match kind.sort() {
    //             &KindSort::Ty(ty) => self.ty_wf(ty),
    //             _ => Ok(Ty::ty_kind(self.kind_wf(kind)?)),
    //         },
    //     }
    // }

    pub fn kind_wf(&mut self, kind: Kind) -> TyResult<Kind> {
        match kind.sort() {
            KindSort::Ty(_) => unreachable!(),
            &KindSort::Abs(param, body) => self.kind_wf(param).and(self.kind_wf(body)),
            // TODO: Check context contains kind?
            &KindSort::Var(_) => Ok(kind),
            &KindSort::Ex(ex) => {
                if self.ascend_ctx(|ctx| ctx.get_kind_ex(ex)).is_some()
                    || self.global_ctx.has_kind_ex(ex)
                {
                    Ok(kind)
                } else {
                    self.kind_illformed(kind)
                }
            },
            &KindSort::Forall(var, body) => self.under_ctx(|this| {
                this.add_kind_var(var);
                this.kind_wf(body)
            }),
        }
    }

    /// Substitute all occurrences of universally quantified type inside it body
    pub fn open_forall(&mut self, ty: Ty, _subst: Ty) -> Ty {
        match ty.kind() {
            &TyKind::Forall(alpha, body) => ty.substitute(alpha, body),
            _ => unreachable!(),
        }
    }

    /// Solves number (int or float) existentials
    ///  to defaults in current context (without ascending)
    pub fn default_number_exes(&mut self) {
        verbose!("Default number existentials");

        let default_int = Ty::default_int();
        let default_float = Ty::default_float();

        self.ctx_mut().int_exes().iter().for_each(|&ex| {
            if let Some(_) = self.get_solution(ex) {
                return;
            }
            verbose!("Default int ex {ex}");
            self.solve(ex, default_int.mono());
        });

        self.ctx_mut().float_exes().iter().for_each(|&ex| {
            if let Some(_) = self.get_solution(ex) {
                return;
            }
            verbose!("Default float ex {ex}");
            self.solve(ex, default_float.mono());
        });
    }

    /// Gets function type DefId applying context to substitute existentials.
    pub fn deep_func_def_id(&self, ty: Ty) -> Option<DefId> {
        let mut maybe_func_ty = ty;
        while let &TyKind::Forall(_, ty) = ty.apply_ctx(self).kind() {
            maybe_func_ty = ty;
        }
        maybe_func_ty.func_def_id()
    }

    // Debug //
    fn dump_ctx_stack(&self) -> String {
        let exes = self.ctx_stack.iter().fold(vec![], |exes, ctx| {
            exes.into_iter()
                .chain(ctx.existentials().into_iter())
                .collect()
        });

        let exes = exes.into_iter().fold((vec![], vec![]), |mut exes, &ex| {
            if let Some(sol) = self.get_solution(ex) {
                exes.1.push((ex, sol));
            } else {
                exes.0.push(ex);
            }
            exes
        });

        let vars = self
            .ctx_stack
            .iter()
            .fold(vec![], |vars, ctx| {
                vars.into_iter().chain(ctx.vars().iter().copied()).collect()
            })
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        let solved = exes
            .1
            .into_iter()
            .map(|(ex, sol)| format!("  {} = {}", ex, sol))
            .collect::<Vec<_>>()
            .join("\n");

        let unsolved = exes
            .0
            .iter()
            .map(|ex| format!("{ex}"))
            .collect::<Vec<_>>()
            .join(", ");

        let global_solved = self
            .global_ctx
            .solved()
            .iter_enumerated_flat()
            .map(|(ex, sol)| format!("  {} = {}", ex, sol))
            .collect::<Vec<_>>()
            .join("\n");

        let global_exes = self
            .global_ctx
            .existentials()
            .iter()
            .map(|ex| format!("{ex}"))
            .collect::<Vec<_>>()
            .join(", ");

        // let ty_bindings =
        //     self.tyctx()
        //         .ty_bindings()
        //         .iter()
        //         .fold(String::new(), |s, (expr, bindings)| {
        //             format!(
        //                 "{}{}: {{{}}}\n",
        //                 s,
        //                 expr,
        //                 bindings
        //                     .iter_enumerated_flat()
        //                     .map(|(var, ty)| format!(
        //                         "{}: [{}]",
        //                         var,
        //                         ty.iter()
        //                             .map(|ty| ty.to_string())
        //                             .collect::<Vec<_>>()
        //                             .join(", ")
        //                     ))
        //                     .collect::<Vec<_>>()
        //                     .join(", ")
        //             )
        //         });

        let expr_ty_bindings =
            self.tyctx()
                .expr_ty_bindings()
                .iter()
                .fold(String::new(), |s, (expr, bindings)| {
                    let bs = bindings.iter().fold(String::new(), |s, (var, ty)| {
                        format!("{}  {}: {}\n", s, var, ty)
                    });
                    format!("{}{}\n{}", s, expr, bs)
                });

        format!(
            "[CTX]\nvars: {}\nsolved:\n{}\nunsolved: {}\nglobal solved:\n{}\nglobal existentials: {}\nType bindings:\n{}",
            vars,  solved, unsolved, global_solved, global_exes, expr_ty_bindings
        )
    }
}

impl<'hir> AlgoCtx for Typecker<'hir> {
    fn get_solution(&self, ex: Ex) -> Option<Ty> {
        self.ascend_ctx(|ctx| ctx.get_solution(ex))
            .or_else(|| self.global_ctx.get_solution(ex))
    }

    fn get_kind_ex_solution(&self, ex: KindEx) -> Option<Kind> {
        self.ascend_ctx(|ctx| ctx.get_kind_ex_solution(ex))
            .or_else(|| self.global_ctx.get_kind_ex_solution(ex))
    }
}

impl<'hir> Stage<()> for Typecker<'hir> {
    fn run(mut self) -> StageResult<()> {
        let conv = TyConv::new(self.sess);
        let ((), sess) = conv.run()?.merged(&mut self.msg);
        self.sess = sess;

        self.under_ctx(|this| {
            // Add existentials defined in conv for definitions, so initial context is
            // well-formed
            this.tyctx()
                .def_ty_exes()
                .iter()
                .copied()
                .for_each(|(def_id, ex)| {
                    tcdbg!(this, add ex, format!("def#{def_id}"));
                    this.add_ex(ex);
                });

            this.tyctx()
                .def_ty_kind_exes()
                .iter()
                .copied()
                .for_each(|(def_id, kind_ex)| {
                    tcdbg!(this, add kind_ex, format!("def#{def_id}"));
                    this.add_kind_ex(kind_ex);
                });

            this.sess.hir.root().items.clone().iter().for_each(|&item| {
                let res = this.synth_item(item);

                match res {
                    Ok(_) => {},
                    Err(err) => match err {
                        TypeckErr::Check | TypeckErr::LateReport => {
                            panic!("Unreported error in typeck")
                        },
                        TypeckErr::Reported => {},
                    },
                }
            });
        });

        outln!(
            dbg,
            self.sess.writer,
            "Type ctx:\n {}",
            self.dump_ctx_stack()
        );

        self.sess.tyctx.apply_ctx_on_typed_nodes(&self.global_ctx);

        if self.sess.config().typeck_debug() {
            outln!(
                dbg,
                self.sess.writer,
                "Typeck debug tree:\n {}",
                self.dbg.get_string()
            );
        }

        stage_result(self.sess, (), self.msg)
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::{config::config::ConfigBuilder, hir::HIR, session::Session};

//     use super::Typecker<'hir>;

//     fn get_hir() -> HIR {
//         HIR::new()
//     }

//     // fn get_typecker<'a>(hir: &'a HIR) -> Typecker<'hir> {
//     //     Typecker<'hir>::new(Session::new(ConfigBuilder::new().emit()),
// hir)     // }

//     mod substitution {

//         // fn basic() {
//         //     let mut Typecker<'hir> = get_typecker(&get_hir());
//         //     let tyctx = Typecker<'hir>.tyctx_mut();
//         //     let
//         //     Typecker<'hir>.substitute(, subst, with)
//         // }
//     }
// }
