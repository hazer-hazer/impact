use crate::{
    cli::verbose,
    hir::{self, item::ItemId, ty::TyPath, HirId, Path, HIR},
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{
        builtin::Builtin,
        def::{DefId, DefKind, Namespace},
        res::ResKind,
    },
    session::{Session, Stage, StageOutput},
    span::span::{Ident, WithSpan},
    typeck::ty::Subst,
};

use self::{
    ctx::{GlobalCtx, InferCtx},
    ty::{Existential, ExistentialId, ExistentialKind, FloatKind, IntKind, PrimTy, Ty, TyKind},
    tyctx::TyCtx,
};

mod builtin;
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

pub type TyResult<T> = Result<T, TypeckErr>;

pub struct Typecker<'hir> {
    hir: &'hir HIR,

    // Context //
    global_ctx: GlobalCtx,
    ctx_stack: Vec<InferCtx>,
    existential: ExistentialId,
    try_mode: bool,

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
            hir,

            global_ctx: Default::default(),
            ctx_stack: Default::default(),
            existential: ExistentialId::new(0),
            try_mode: false,

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

    fn define_builtins(&mut self) {
        Builtin::each(|bt| {
            let def_id = self
                .sess
                .def_table
                .builtin_def_id(bt)
                .expect(&format!("Builtin {} is not defined in code", bt));

            let ty = self.tyctx().builtin_ty(bt);

            self.tyctx_mut().type_node(HirId::new_owner(def_id), ty);

            // Get builtin definition real ident to have span
            let def_name = self.sess.def_table.get_def(def_id).unwrap().name();

            assert_eq!(def_name.sym(), bt.sym());

            // Only value builtins are typed terms
            match bt.ns() {
                Namespace::Value => {
                    self.type_term(def_name, ty);
                },
                _ => {},
            }

            // verbose!(
            //     "Define builtin `{}` type `{}`",
            //     def_name,
            //     ty
            // );
        });
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
        verbose!("< [EXIT CTX]");

        self.global_ctx
            .add(self.ctx_depth(), self.ctx_stack.pop().unwrap());
    }

    /// Returns try mode flag and context depth before try mode to restore to them
    fn enter_try_mode(&mut self) -> (bool, usize) {
        let restore = (self.try_mode, self.ctx_depth());

        verbose!("?> [ENTER TRY CTX]");

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
        verbose!("> [ENTER CTX]");

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

    fn add_fresh_ex(&mut self, kind: ExistentialKind) -> (Existential, Ty) {
        let ex = self.fresh_ex(kind);
        self.add_ex(ex);
        (ex, Ty::existential(ex))
    }

    fn add_fresh_common_ex(&mut self) -> (Existential, Ty) {
        self.add_fresh_ex(ExistentialKind::Common)
    }

    fn solve(&mut self, ex: Existential, sol: Ty) -> Ty {
        verbose!("Solve {} as {}", ex, sol);
        self._ascend_ctx_mut(|ctx| ctx.solve(ex, sol))
            .map_or_else(|| self.global_ctx.solve(ex, sol), |(ty, _)| Some(ty))
            .unwrap()
    }

    fn add_func_param_sol(&mut self, ex: Existential, sol: Ty) -> Ty {
        verbose!("Add {} func param solution {}", ex, sol);
        self._ascend_ctx_mut(|ctx| ctx.add_func_param_sol(ex, sol))
            .map_or_else(|| todo!("Global ctx func param exes"), |(ty, _)| Some(ty))
            .unwrap()
    }

    fn type_term(&mut self, name: Ident, ty: Ty) {
        self.ctx().type_term(name, ty)
    }

    fn add_ex(&mut self, ex: Existential) {
        self.ctx().add_ex(ex);
    }

    fn add_var(&mut self, name: Ident) -> Ty {
        self.ctx().add_var(name);
        Ty::var(name)
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
            TyKind::Error | TyKind::Unit | TyKind::Var(_) | TyKind::Prim(_) => ty,

            &TyKind::Existential(ex) => self
                .get_solution(ex)
                .map_or(ty, |ty| self._apply_ctx_on(ty)),

            &TyKind::Func(param_ty, return_ty) => {
                let param = self._apply_ctx_on(param_ty);
                let ret = self._apply_ctx_on(return_ty);
                Ty::func(param, ret)
            },

            &TyKind::Forall(ident, body) => {
                let body = self._apply_ctx_on(body);
                Ty::forall(ident, body)
            },
        }
    }

    pub fn ty_occurs_in(&mut self, ty: Ty, name: Subst) -> bool {
        match ty.kind() {
            TyKind::Error | TyKind::Unit | TyKind::Prim(_) => false,
            &TyKind::Var(name_) if name == name_ => true,
            TyKind::Var(_) => false,
            &TyKind::Existential(ex) if name == ex => {
                // TODO: Is this right?!
                true
            },
            TyKind::Existential(_) => false,
            &TyKind::Func(param, ret) => {
                self.ty_occurs_in(param, name) || self.ty_occurs_in(ret, name)
            },
            &TyKind::Forall(alpha, _) if name == alpha => true,
            &TyKind::Forall(_, body) => self.ty_occurs_in(body, name),
        }
    }

    pub fn ty_wf(&mut self, ty: Ty) -> TyResult<Ty> {
        match ty.kind() {
            TyKind::Error => Ok(ty),
            TyKind::Unit | TyKind::Prim(_) => Ok(ty),
            &TyKind::Var(ident) => {
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
            &TyKind::Func(param_ty, return_ty) => {
                self.ty_wf(param_ty)?;
                self.ty_wf(return_ty)
            },
            &TyKind::Forall(alpha, body) => self.under_ctx(InferCtx::new_with_var(alpha), |this| {
                // let open_forall = this.open_forall(body, alpha);
                this.ty_wf(body)
            }),
        }
    }

    /// Substitute all occurrences of universally quantified type inside it body
    pub fn open_forall(&mut self, ty: Ty, _subst: Ty) -> Ty {
        match ty.kind() {
            &TyKind::Forall(alpha, body) => self.substitute(ty, Subst::Name(alpha), body),
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
            self.solve(ex, default_int);
        });

        self.ctx().float_exes().iter().for_each(|&ex| {
            if let Some(_) = self.get_solution(ex) {
                return;
            }
            verbose!("Default float ex {}", ex);
            self.solve(ex, default_float);
        });
    }

    // Substitution //
    pub fn substitute(&mut self, ty: Ty, subst: Subst, with: Ty) -> Ty {
        verbose!("Substitute {} in {} with {}", subst, ty, with);

        match ty.kind() {
            TyKind::Error | TyKind::Unit | TyKind::Prim(_) => ty,
            &TyKind::Var(ident) => {
                if subst == ident {
                    with
                } else {
                    ty
                }
            },
            &TyKind::Existential(ex) => {
                if subst == ex {
                    with
                } else {
                    ty
                }
            },
            &TyKind::Func(param_ty, return_ty) => {
                let param = self.substitute(param_ty, subst, with);
                let ret = self.substitute(return_ty, subst, with);
                Ty::func(param, ret)
            },
            &TyKind::Forall(ident, body) => {
                if subst == ident {
                    Ty::forall(ident, with)
                } else {
                    let subst = self.substitute(body, subst, with);
                    Ty::forall(ident, subst)
                }
            },
        }
    }

    // Conversion //
    fn conv(&mut self, ty: hir::ty::Ty) -> Ty {
        let ty = self.hir.ty(ty);
        // TODO: Allow recursive?

        match ty.kind {
            hir::ty::TyKind::Path(TyPath(path)) => self.conv_path(path),
            hir::ty::TyKind::Func(param, body) => {
                let param = self.conv(param);
                let ret = self.conv(body);
                Ty::func(param, ret)
            },
        }
    }

    fn conv_path(&mut self, path: Path) -> Ty {
        let path = self.hir.path(path);
        match path.res().kind() {
            &ResKind::Def(def_id) => {
                let def = self.sess.def_table.get_def(def_id).unwrap();
                match def.kind() {
                    DefKind::TyAlias => {
                        // Path conversion is done linearly, i.e. we get type alias from HIR and convert its type, caching it
                        // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                        self.conv_ty_alias(def_id)
                    },

                    // Non-type definitions from type namespace
                    DefKind::Root | DefKind::Mod => {
                        MessageBuilder::error()
                            .span(path.span())
                            .text(format!("{} item used as type", def.kind()))
                            .emit_single_label(self);

                        Ty::error()
                    },

                    // Definitions from value namespace
                    DefKind::Builtin(_)
                    | DefKind::DeclareBuiltin
                    | DefKind::Func
                    | DefKind::Var => unreachable!(),
                }
            },
            &ResKind::Builtin(bt) if bt.is_ty() => self.tyctx().builtin_ty(bt),
            _ => unreachable!(),
        }
    }

    fn conv_ty_alias(&mut self, ty_alias_def_id: DefId) -> Ty {
        let hir_id = HirId::new_owner(ty_alias_def_id);
        let def = self.sess.def_table.get_def(ty_alias_def_id).unwrap();
        if let Some(def_ty) = self.tyctx().node_type(hir_id) {
            return def_ty;
        }

        let ty_alias = self.hir.item(ItemId::new(def.def_id.into())).ty_alias();
        let ty = self.conv(ty_alias.ty);
        self.tyctx_mut().type_node(hir_id, ty);
        ty
    }

    fn conv_int_kind(&mut self, kind: hir::expr::IntKind) -> Ty {
        match kind {
            hir::expr::IntKind::Unknown => return self.add_fresh_ex(ExistentialKind::Int).1,
            _ => {},
        }

        Ty::prim(PrimTy::Int(match kind {
            hir::expr::IntKind::U8 => IntKind::U8,
            hir::expr::IntKind::U16 => IntKind::U16,
            hir::expr::IntKind::U32 => IntKind::U32,
            hir::expr::IntKind::U64 => IntKind::U64,
            hir::expr::IntKind::Uint => IntKind::Uint,
            hir::expr::IntKind::I8 => IntKind::I8,
            hir::expr::IntKind::I16 => IntKind::I16,
            hir::expr::IntKind::I32 => IntKind::I32,
            hir::expr::IntKind::I64 => IntKind::I64,
            hir::expr::IntKind::Int => IntKind::Int,
            hir::expr::IntKind::Unknown => unreachable!(),
        }))
    }

    fn conv_float_kind(&mut self, kind: hir::expr::FloatKind) -> Ty {
        match kind {
            hir::expr::FloatKind::Unknown => self.add_fresh_ex(ExistentialKind::Float).1,
            hir::expr::FloatKind::F32 => Ty::prim(PrimTy::Float(FloatKind::F32)),
            hir::expr::FloatKind::F64 => Ty::prim(PrimTy::Float(FloatKind::F64)),
        }
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

        format!(
            "[CTX]\nvars: {}\nterms:\n{}\nsolved:\n{}\nunsolved: {}\nglobal solved:\n{}\nglobal existentials: {}\n",
            vars, terms, solved, unsolved, global_solved, global_exes
        )
    }
}

impl<'ast> Stage<()> for Typecker<'ast> {
    fn run(mut self) -> StageOutput<()> {
        self.under_new_ctx(|this| {
            this.define_builtins();

            this.hir.root().items.iter().for_each(|&item| {
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

        StageOutput::new(self.sess, (), self.msg)
    }
}

#[cfg(test)]
mod tests {
    use crate::{config::config::ConfigBuilder, hir::HIR, session::Session};

    use super::Typecker;

    fn get_hir() -> HIR {
        HIR::new()
    }

    fn get_typecker<'a>(hir: &'a HIR) -> Typecker<'a> {
        Typecker::new(Session::new(ConfigBuilder::new().emit()), hir)
    }

    mod substitution {

        // fn basic() {
        //     let mut typecker = get_typecker(&get_hir());
        //     let tyctx = typecker.tyctx_mut();
        //     let
        //     typecker.substitute(, subst, with)
        // }
    }
}
