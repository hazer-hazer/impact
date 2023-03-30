use std::fmt::Display;

use crate::{
    cli::verbose,
    hir::{
        self,
        item::{ItemId, TyAlias},
        ty::TyPath,
        HirId, Path, Res, HIR,
    },
    interface::writer::outln,
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{
        builtin::Builtin,
        def::{DefId, DefKind},
    },
    session::{Session, Stage, StageOutput},
    span::span::{Ident, WithSpan},
    typeck::ty::Subst,
};

use self::{
    ctx::{GlobalCtx, InferCtx},
    ty::{
        ExPair, Existential, ExistentialId, ExistentialKind, FloatKind, IntKind, MonoTy, Ty,
        TyKind, TyVarId,
    },
    tyctx::TyCtx,
};

pub mod builtin;
mod check;
pub mod ctx;
mod synth;
pub mod ty;
pub mod tyctx;

#[derive(Debug)]
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
    // Context //
    global_ctx: GlobalCtx,
    ctx_stack: Vec<InferCtx>,
    existential: ExistentialId,
    try_mode: bool,

    hir: &'hir HIR,

    //
    msg: MessageStorage,
    sess: Session,
}

impl<'hir> MessageHolder for Typecker<'hir> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'hir> Typecker<'hir> {
    pub fn new(sess: Session, hir: &'hir HIR) -> Self {
        Self {
            global_ctx: Default::default(),
            ctx_stack: Default::default(),
            existential: ExistentialId::new(0),
            try_mode: false,

            hir,

            sess,
            msg: Default::default(),
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

    // fn cyclic_ty(&mut self, ty: Ty, references: Ty, on: ExistentialId) {
    //     MessageBuilder::error()
    //     .text(format!("Cyclic type {}", self.tyctx().ty(ty)))
    //     .label(span, text)
    // }

    // Context //
    fn ctx(&mut self) -> &mut InferCtx {
        self.ctx_stack.last_mut().unwrap()
    }

    fn ctx_depth(&self) -> usize {
        self.ctx_stack.len()
    }

    fn enter_ctx(&mut self, ctx: InferCtx) {
        self.ctx_stack.push(ctx);
    }

    fn exit_ctx(&mut self) {
        // assert!(self.ctx().unsolved().is_empty());
        verbose!("< [EXIT CTX {}]", self.ctx_depth());

        let depth = self.ctx_depth();

        self.global_ctx.add(depth, self.ctx_stack.pop().unwrap());
    }

    /// Returns try mode flag and context depth before try mode to restore to them
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

    pub fn under_ctx<T>(&mut self, ctx: InferCtx, mut f: impl FnMut(&mut Self) -> T) -> T {
        verbose!("> [ENTER CTX {}]", self.ctx_depth() + 1);

        self.enter_ctx(ctx);
        let res = f(self);
        self.exit_ctx();

        res
    }

    pub fn under_new_ctx<T>(&mut self, f: impl FnMut(&mut Self) -> T) -> T {
        self.under_ctx(InferCtx::default(), f)
    }

    fn try_to<T>(&mut self, mut f: impl FnMut(&mut Self) -> TyResult<T>) -> TyResult<T> {
        let restore = self.enter_try_mode();
        let res = f(self);
        self.exit_try_mode(res, restore)
    }

    /// Goes up from current context to the root looking for something in each scope
    /// Returns result of the callback (if some returned) and depth of context scope
    /// where callback returned result
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
    fn find_unbound_ex_depth(&self, ex: Existential) -> (usize, usize) {
        self._ascend_ctx(|ctx| {
            if let Some(pos) = ctx.get_ex_index(ex) {
                Some(pos)
            } else {
                None
            }
        })
        .or_else(|| self.global_ctx.get_ex_index(ex))
        .expect(&format!(
            "Undefined existential {}; ctx {}",
            ex,
            self.dump_ctx_stack()
        ))
    }

    fn fresh_ex(&mut self, kind: ExistentialKind) -> Existential {
        Existential::new(kind, *self.existential.inc())
    }

    fn add_fresh_ex(&mut self, kind: ExistentialKind) -> ExPair {
        let ex = self.fresh_ex(kind);
        self.add_ex(ex);
        (ex, Ty::existential(ex))
    }

    fn add_fresh_common_ex(&mut self) -> ExPair {
        self.add_fresh_ex(ExistentialKind::Common)
    }

    fn add_fresh_common_ex_list(&mut self, count: usize) -> Vec<ExPair> {
        (0..count)
            .into_iter()
            .map(|_| self.add_fresh_common_ex())
            .collect()
    }

    fn solve(&mut self, ex: Existential, sol: MonoTy) -> Ty {
        let sol = sol.ty;
        if let &TyKind::Existential(sol_ex) = sol.kind() {
            assert_ne!(sol_ex, ex, "Tried to solve ex with itself {} / {}", ex, sol);
        }
        verbose!("Solve {} as {}", ex, sol);
        self._ascend_ctx_mut(|ctx| ctx.solve(ex, sol))
            .map_or_else(|| self.global_ctx.solve(ex, sol), |(ty, _)| Some(ty))
            .unwrap()
    }

    fn add_func_param_sol(&mut self, ex: Existential, sol: MonoTy) -> Ty {
        let sol = sol.ty;
        verbose!("Add {} func param solution {}", ex, sol);
        self._ascend_ctx_mut(|ctx| ctx.add_func_param_sol(ex, sol))
            .map_or_else(|| todo!("Global ctx func param exes"), |(ty, _)| Some(ty))
            .unwrap()
    }

    fn type_term(&mut self, name: Ident, ty: Ty) {
        verbose!("Type term {}: {}", name, ty);
        self.ctx().type_term(name, ty)
    }

    fn add_ex(&mut self, ex: Existential) {
        verbose!("Add ex {}", ex);
        self.ctx().add_ex(ex);
    }

    fn add_var(&mut self, var: TyVarId) -> Ty {
        verbose!("Add var {}", var);
        self.ctx().add_var(var);
        Ty::var(var)
    }

    fn get_solution(&self, ex: Existential) -> Option<Ty> {
        self.ascend_ctx(|ctx| ctx.get_solution(ex))
            .or_else(|| self.global_ctx.get_solution(ex))
    }

    fn is_unsolved(&self, ex: Existential) -> bool {
        self.get_solution(ex).is_none()
    }

    fn lookup_typed_term_ty(&self, name: Ident) -> Option<Ty> {
        self.ascend_ctx(|ctx: &InferCtx| ctx.get_term(name))
    }

    fn apply_ctx_on(&self, ty: Ty) -> Ty {
        let res = self._apply_ctx_on(ty);
        if ty != res {
            verbose!("[APPLY CTX] {} => {}", ty, res);
        }
        res
    }

    fn _apply_ctx_on(&self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Var(_)
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str => ty,

            &TyKind::Existential(ex) => self
                .get_solution(ex)
                .map_or(ty, |ty| self._apply_ctx_on(ty)),

            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                let params = params
                    .iter()
                    .copied()
                    .map(|param| self._apply_ctx_on(param))
                    .collect();
                let body = self._apply_ctx_on(*body);
                Ty::func(ty.func_def_id(), params, body)
            },

            &TyKind::Forall(ident, body) => {
                let body = self._apply_ctx_on(body);
                Ty::forall(ident, body)
            },
            &TyKind::Ref(inner) => Ty::ref_to(self._apply_ctx_on(inner)),
            // TODO: Review
            TyKind::HigherKinded(_, _) => ty,
        }
    }

    pub fn ty_occurs_in(&mut self, ty: Ty, name: Subst) -> bool {
        match ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Bool
            | TyKind::Int(_)
            | TyKind::Float(_)
            | TyKind::Str => false,
            &TyKind::Var(name_) if name == name_ => true,
            TyKind::Var(_) => false,
            &TyKind::Existential(ex) if name == ex => {
                // TODO: Is this right?!
                true
            },
            TyKind::Existential(_) => false,
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                params
                    .iter()
                    .copied()
                    .any(|param| self.ty_occurs_in(param, name))
                    || self.ty_occurs_in(*body, name)
            },
            &TyKind::Forall(alpha, _) if name == alpha => true,
            &TyKind::Forall(_, body) => self.ty_occurs_in(body, name),
            &TyKind::Ref(inner) => self.ty_occurs_in(inner, name),
            // TODO: Review
            &TyKind::HigherKinded(domain, range) => {
                self.ty_occurs_in(domain, name) || self.ty_occurs_in(range, name)
            },
        }
    }

    pub fn ty_wf(&mut self, ty: Ty) -> TyResult<Ty> {
        match ty.kind() {
            TyKind::Error => Ok(ty),
            TyKind::Unit | TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) | TyKind::Str => Ok(ty),
            &TyKind::Var(_ident) => {
                // FIXME
                Ok(ty)
                // if self.ascend_ctx(|ctx| ctx.get_var(ident)).is_some() {
                //     Ok(ty)
                // } else {
                //     self.ty_illformed(ty)
                // }
            },
            &TyKind::Existential(ex) => {
                if self.ascend_ctx(|ctx| ctx.get_ex(ex)).is_some() || self.global_ctx.has_ex(ex) {
                    Ok(ty)
                } else {
                    self.ty_illformed(ty)
                }
            },
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => {
                params.iter().copied().try_for_each(|param| {
                    self.ty_wf(param)?;
                    Ok(())
                })?;
                self.ty_wf(*body)
            },
            &TyKind::Forall(alpha, body) => self.under_ctx(InferCtx::new_with_var(alpha), |this| {
                // let open_forall = this.open_forall(body, alpha);
                this.ty_wf(body)
            }),
            &TyKind::Ref(inner) => self.ty_wf(inner),
        }
    }

    /// Substitute all occurrences of universally quantified type inside it body
    pub fn open_forall(&mut self, ty: Ty, _subst: Ty) -> Ty {
        match ty.kind() {
            &TyKind::Forall(alpha, body) => ty.substitute(Subst::Var(alpha), body),
            _ => unreachable!(),
        }
    }

    /// Solves number (int or float) existentials
    ///  to defaults in current context (without ascending)
    pub fn default_number_exes(&mut self) {
        verbose!("Default number existentials");

        let default_int = Ty::default_int();
        let default_float = Ty::default_float();

        self.ctx().int_exes().iter().for_each(|&ex| {
            if let Some(_) = self.get_solution(ex) {
                return;
            }
            verbose!("Default int ex {}", ex);
            self.solve(ex, default_int.mono());
        });

        self.ctx().float_exes().iter().for_each(|&ex| {
            if let Some(_) = self.get_solution(ex) {
                return;
            }
            verbose!("Default float ex {}", ex);
            self.solve(ex, default_float.mono());
        });
    }

    /// Gets function type DefId applying context to substitute existentials.
    pub fn deep_func_def_id(&self, ty: Ty) -> Option<DefId> {
        self.apply_ctx_on(ty).func_def_id()
    }

    // Conversion //
    fn conv(&mut self, ty: hir::ty::Ty) -> Ty {
        let ty = self.hir.ty(ty);
        // TODO: Allow recursive?

        match ty.kind() {
            &hir::ty::TyKind::Path(TyPath(path)) => self.conv_ty_path(path),
            hir::ty::TyKind::Func(params, body) => {
                let params = params
                    .iter()
                    .copied()
                    .map(|param| self.conv(param))
                    .collect();
                let body = self.conv(*body);
                Ty::func(None, params, body)
            },
            hir::ty::TyKind::App(cons, args) => match self.hir.ty(*cons).kind() {
                hir::ty::TyKind::Builtin(bt) => match bt {
                    Builtin::RefTy => {
                        let mut args = args.iter().map(|&arg| self.conv(arg)).collect::<Vec<_>>();
                        assert_eq!(args.len(), 1);
                        return Ty::ref_to(args.remove(0));
                    },
                    _ => panic!(),
                },
                _ => panic!(),
            },
            hir::ty::TyKind::Builtin(bt) => match bt {
                Builtin::UnitTy => Ty::unit(),
                Builtin::I32 => Ty::int(IntKind::I32),
                Builtin::Str => Ty::str(),
                Builtin::RefTy => {
                    // FIXME: Remove and return constructor type. KINDS AAAAAAA
                    MessageBuilder::error()
                        .span(ty.span())
                        .text(format!(
                            "{} is a type constructor and cannot be used without parameter",
                            bt
                        ))
                        .emit_single_label(self);
                    Ty::error()
                },
                Builtin::RefCons | Builtin::AddInt | Builtin::SubInt | Builtin::UnitValue => {
                    unreachable!()
                },
            },
        }
    }

    fn conv_ty_path(&mut self, path: Path) -> Ty {
        let path = self.hir.path(path);
        match path.res() {
            &Res::Def(def_kind, def_id) => {
                match def_kind {
                    DefKind::TyAlias => {
                        // Path conversion is done linearly, i.e. we get type alias from HIR and convert its type, caching it
                        // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                        self.conv_ty_alias(def_id)
                    },

                    // Non-type definitions from type namespace
                    DefKind::Root | DefKind::Mod => {
                        MessageBuilder::error()
                            .span(path.span())
                            .text(format!("{} item used as type", def_kind))
                            .emit_single_label(self);

                        Ty::error()
                    },

                    DefKind::DeclareBuiltin => todo!(),

                    // Definitions from value namespace
                    DefKind::External
                    | DefKind::Local
                    | DefKind::Lambda
                    | DefKind::Func
                    | DefKind::Value => {
                        unreachable!()
                    },
                }
            },
            &Res::Builtin(bt) if bt.is_ty() => todo!(),
            _ => unreachable!(),
        }
    }

    fn conv_ty_alias(&mut self, ty_alias_def_id: DefId) -> Ty {
        let hir_id = HirId::new_owner(ty_alias_def_id);
        let def = self.sess.def_table.get_def(ty_alias_def_id);
        if let Some(def_ty) = self.tyctx().node_type(hir_id) {
            return def_ty;
        }

        let &TyAlias { ty } = self.hir.item(ItemId::new(def.def_id.into())).ty_alias();
        let ty = self.conv(ty);
        self.tyctx_mut().type_node(hir_id, ty);
        ty
    }

    fn conv_int(&mut self, kind: hir::expr::IntKind) -> Ty {
        IntKind::try_from(kind).map_or_else(
            |_| self.add_fresh_ex(ExistentialKind::Int).1,
            |kind| Ty::int(kind),
        )
    }

    fn conv_float(&mut self, kind: hir::expr::FloatKind) -> Ty {
        FloatKind::try_from(kind).map_or_else(
            |_| self.add_fresh_ex(ExistentialKind::Float).1,
            |kind| Ty::float(kind),
        )
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

        let terms = self
            .ctx_stack
            .iter()
            .fold(vec![], |terms, ctx| {
                terms.into_iter().chain(ctx.terms().into_iter()).collect()
            })
            .iter()
            .map(|(name, ty)| format!("  {}: {}", name, ty))
            .collect::<Vec<_>>()
            .join("\n");

        let solved = exes
            .1
            .into_iter()
            .map(|(ex, sol)| format!("  {} = {}", ex, sol))
            .collect::<Vec<_>>()
            .join("\n");

        let unsolved = exes
            .0
            .iter()
            .map(|ex| format!("{}", ex))
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
            .map(|ex| format!("{}", ex))
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
            "[CTX]\nvars: {}\nterms:\n{}\nsolved:\n{}\nunsolved: {}\nglobal solved:\n{}\nglobal existentials: {}\nType bindings:\n{}",
            vars, terms, solved, unsolved, global_solved, global_exes, expr_ty_bindings
        )
    }
}

impl<'hir> Stage<()> for Typecker<'hir> {
    fn run(mut self) -> StageOutput<()> {
        self.under_new_ctx(|this| {
            this.hir.root().items.clone().iter().for_each(|&item| {
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

        outln!(self.sess.writer, "Type ctx:\n {}", self.dump_ctx_stack());

        self.sess.tyctx.apply_ctx_on_typed_nodes(&self.global_ctx);

        StageOutput::new(self.sess, (), self.msg)
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
//     //     Typecker<'hir>::new(Session::new(ConfigBuilder::new().emit()), hir)
//     // }

//     mod substitution {

//         // fn basic() {
//         //     let mut Typecker<'hir> = get_typecker(&get_hir());
//         //     let tyctx = Typecker<'hir>.tyctx_mut();
//         //     let
//         //     Typecker<'hir>.substitute(, subst, with)
//         // }
//     }
// }
