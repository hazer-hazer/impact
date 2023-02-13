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
    ctx::InferCtx,
    ty::{
        Existential, ExistentialId, ExistentialKind, FloatKind, IntKind, PrimTy, Ty, TyError,
        TyKind, TyResult,
    },
    tyctx::TyCtx,
};

mod builtin;
mod check;
pub mod ctx;
mod synth;
pub mod ty;
pub mod tyctx;

pub struct Typecker<'hir> {
    hir: &'hir HIR,

    // Context //
    ctx_stack: Vec<InferCtx>,
    try_mode: bool,
    existential: ExistentialId,

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

            ctx_stack: Default::default(),
            try_mode: false,
            existential: ExistentialId::new(0),

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
            //     self.tyctx().pp(ty)
            // );
        });
    }

    // Errors //
    fn ty_illformed(&mut self, ty: Ty) {
        panic!("Illformed type {}", self.tyctx().ty(ty))
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

        self.ctx_stack.pop();
    }

    /// Returns try mode flag and context depth before try mode to restore to them
    fn enter_try_mode(&mut self) -> (bool, usize) {
        let restore = (self.try_mode, self.ctx_depth());
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
        let mut ctx_index = self.ctx_stack.len() - 1;

        loop {
            let res = f(&self.ctx_stack[ctx_index]);
            if let Some(res) = res {
                return Some((res, ctx_index));
            }

            if ctx_index == 0 {
                break;
            }

            ctx_index -= 1;
        }

        None
    }

    fn _ascend_ctx_mut<T>(
        &mut self,
        mut f: impl FnMut(&mut InferCtx) -> Option<T>,
    ) -> Option<(T, usize)> {
        let mut ctx_index = self.ctx_stack.len() - 1;

        loop {
            let res = f(&mut self.ctx_stack[ctx_index]);
            if let Some(res) = res {
                return Some((res, ctx_index));
            }

            if ctx_index == 0 {
                break;
            }

            ctx_index -= 1;
        }

        None
    }

    fn ascend_ctx<T>(&self, f: impl FnMut(&InferCtx) -> Option<T>) -> Option<T> {
        self._ascend_ctx(f).map_or(None, |(val, _depth)| Some(val))
    }

    fn find_unbound_ex_depth(&self, ex: Existential) -> usize {
        self._ascend_ctx(|ctx| if ctx.has_ex(ex) { Some(()) } else { None })
            .expect("Undefined existential id")
            .1
    }

    fn fresh_ex(&mut self, kind: ExistentialKind) -> Existential {
        Existential::new(kind, *self.existential.inc())
    }

    fn add_fresh_ex(&mut self, kind: ExistentialKind) -> (Existential, Ty) {
        let ex = self.fresh_ex(kind);
        self.add_ex(ex);
        (ex, self.tyctx_mut().existential(ex))
    }

    fn add_fresh_common_ex(&mut self) -> (Existential, Ty) {
        self.add_fresh_ex(ExistentialKind::Common)
    }

    fn solve(&mut self, ex: Existential, solution: Ty) -> Ty {
        verbose!("Solve {} as {}", ex, self.tyctx().pp(solution));
        self._ascend_ctx_mut(|ctx| ctx.solve(ex, solution))
            .map_or(None, |(ty, _)| Some(ty))
            .unwrap()
    }

    fn type_term(&mut self, name: Ident, ty: Ty) {
        self.ctx().type_term(name, ty)
    }

    fn add_ex(&mut self, ex: Existential) {
        self.ctx().add_ex(ex);
    }

    fn add_var(&mut self, name: Ident) {
        self.ctx().add_var(name);
    }

    fn lookup_typed_term_ty(&self, name: Ident) -> Option<Ty> {
        self.ascend_ctx(|ctx: &InferCtx| ctx.get_term(name))
    }

    fn apply_ctx_on(&mut self, ty: Ty) -> Ty {
        let res = self._apply_ctx_on(ty);
        if ty != res {
            verbose!(
                "[APPLY CTX] {} => {}",
                self.tyctx().pp(ty),
                self.tyctx().pp(res)
            );
        }
        res
    }

    fn _apply_ctx_on(&mut self, ty: Ty) -> Ty {
        match self.tyctx().ty(ty).kind() {
            TyKind::Error | TyKind::Unit | TyKind::Var(_) | TyKind::Prim(_) => ty,

            &TyKind::Existential(ex) => self
                .ascend_ctx(|ctx| ctx.get_solution(ex))
                .map_or(ty, |ty| self._apply_ctx_on(ty)),

            &TyKind::Func(param_ty, return_ty) => {
                let param = self._apply_ctx_on(param_ty);
                let ret = self._apply_ctx_on(return_ty);
                self.tyctx_mut().func(param, ret)
            },

            &TyKind::Forall(ident, body) => {
                let body = self._apply_ctx_on(body);
                self.tyctx_mut().forall(ident, body)
            },
        }
    }

    pub fn ty_occurs_in(&mut self, ty: Ty, name: Subst) -> bool {
        match self.tyctx().ty(ty).kind() {
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

    pub fn ty_wf(&mut self, ty: Ty) -> TyResult<()> {
        match self.tyctx().ty(ty).kind() {
            TyKind::Unit | TyKind::Prim(_) => Ok(()),
            &TyKind::Var(ident) => {
                if self.ascend_ctx(|ctx| ctx.get_var(ident)).is_some() {
                    Ok(())
                } else {
                    self.ty_illformed(ty);
                    Err(TyError())
                }
            },
            &TyKind::Existential(ex) => {
                if self.ascend_ctx(|ctx| ctx.get_ex(ex)).is_some() {
                    Ok(())
                } else {
                    self.ty_illformed(ty);
                    Err(TyError())
                }
            },
            &TyKind::Func(param_ty, return_ty) => {
                self.ty_wf(param_ty)?;
                self.ty_wf(return_ty)
            },
            &TyKind::Forall(alpha, body) => self.under_new_ctx(|this| {
                let alpha = this.tyctx_mut().var(alpha);
                let open_forall = this.open_forall(body, alpha);
                this.ty_wf(open_forall)?;
                Ok(())
            }),
            _ => {
                self.ty_illformed(ty);
                Err(TyError())
            },
        }
    }

    /// Substitute all occurrences of universally quantified type inside it body
    pub fn open_forall(&mut self, ty: Ty, _subst: Ty) -> Ty {
        match self.tyctx().ty(ty).kind() {
            &TyKind::Forall(alpha, body) => self.substitute(ty, Subst::Name(alpha), body),
            _ => unreachable!(),
        }
    }

    /// Solves number (int or float) existentials
    ///  to defaults in current context (without ascending)
    pub fn default_number_exes(&mut self) {
        verbose!("Default number existentials");

        let default_int = self.tyctx_mut().default_int();
        let default_float = self.tyctx_mut().default_float();

        self.ctx().int_exes().iter().for_each(|&ex| {
            verbose!("Default int ex {}", ex);
            self.solve(ex, default_int);
        });

        self.ctx().float_exes().iter().for_each(|&ex| {
            verbose!("Default float ex {}", ex);
            self.solve(ex, default_float);
        });
    }

    // Substitution //
    pub fn substitute(&mut self, ty: Ty, subst: Subst, with: Ty) -> Ty {
        verbose!(
            "Substitute {} in {} with {}",
            subst,
            self.tyctx().pp(ty),
            self.tyctx().pp(with)
        );

        match self.tyctx().ty(ty).kind() {
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
                self.tyctx_mut().func(param, ret)
            },
            &TyKind::Forall(ident, body) => {
                if subst == ident {
                    self.tyctx_mut().forall(ident, with)
                } else {
                    let subst = self.substitute(body, subst, with);
                    self.tyctx_mut().forall(ident, subst)
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
                self.tyctx_mut().func(param, ret)
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

                        self.tyctx_mut().error()
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

        self.tyctx_mut().prim(PrimTy::Int(match kind {
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
            hir::expr::FloatKind::F32 => self.tyctx_mut().prim(PrimTy::Float(FloatKind::F32)),
            hir::expr::FloatKind::F64 => self.tyctx_mut().prim(PrimTy::Float(FloatKind::F64)),
        }
    }
}

impl<'ast> Stage<()> for Typecker<'ast> {
    fn run(mut self) -> StageOutput<()> {
        self.under_new_ctx(|this| {
            this.define_builtins();

            this.hir.root().items.iter().for_each(|&item| {
                let _ = this.synth_item(item);
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
