use crate::{
    cli::verbose,
    hir::{
        self,
        expr::{Block, Call, Expr, ExprKind, Lambda, Lit, TyExpr},
        item::{ItemId, ItemKind, Mod},
        stmt::{Stmt, StmtKind},
        ty::TyPath,
        HirId, Path, HIR,
    },
    message::message::{Message, MessageBuilder, MessageHolder, MessageStorage},
    resolve::{
        builtin::Builtin,
        def::{DefId, DefKind, Namespace},
        res::ResKind,
    },
    session::{Session, Stage, StageOutput},
    span::span::{Ident, Spanned, WithSpan},
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
pub mod ctx;
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

    // pub fn get_int_ex_solution(&self, ex: Existential) -> Option<Ty> {
    //     self.ascend_ctx(|ctx| ctx.get_solution(ex)).map(|sol| {
    //         // FIXME: Move to tyctx helper `int_ex_solution`
    //         let ty = self.tyctx().ty(sol);
    //         match ty.kind() {
    //             TyKind::Prim(PrimTy::Int(_) | PrimTy::IntEx(_))
    //             // | TyKind::Existential(_)
    //             => sol,
    //             _ => panic!(
    //                 "Unexpected solution of integer existential {}",
    //                 self.tyctx().pp(sol)
    //             ),
    //         }
    //     })
    // }

    // pub fn get_float_ex_solution(&self, ex: Existential) -> Option<Ty> {
    //     self.ascend_ctx(|ctx| ctx.get_solution(ex)).map(|sol| {
    //         // FIXME: Move to tyctx helper `float_ex_solution`
    //         let ty = self.tyctx().ty(sol);
    //         match ty.kind() {
    //             TyKind::Prim(PrimTy::Float(_) | PrimTy::FloatEx(_))
    //             // | TyKind::Existential(_)
    //             => sol,
    //             _ => panic!(
    //                 "Unexpected solution of integer existential {}",
    //                 self.tyctx().pp(sol)
    //             ),
    //         }
    //     })
    // }

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

    // Synthesis //
    fn synth_expr(&mut self, expr_id: Expr) -> TyResult<Ty> {
        verbose!("Synth type of expression {}", expr_id);

        let expr = self.hir.expr(expr_id);
        let expr_ty = match expr.kind() {
            ExprKind::Lit(lit) => self.synth_lit(lit),
            ExprKind::Path(path) => self.synth_path(path.0),
            &ExprKind::Block(block) => self.synth_block(block),
            ExprKind::Lambda(lambda) => self.synth_lambda(lambda),
            ExprKind::Call(call) => self.synth_call(call),
            &ExprKind::Let(block) => self.under_new_ctx(|this| this.synth_block(block)),
            ExprKind::Ty(ty_expr) => self.synth_ty_expr(ty_expr),
        }?;

        let expr_ty = self.apply_ctx_on(expr_ty);

        self.tyctx_mut().type_node(expr_id, expr_ty);

        Ok(expr_ty)
    }

    fn synth_lit(&mut self, lit: &Lit) -> TyResult<Ty> {
        let prim = match lit {
            Lit::Bool(_) => PrimTy::Bool,
            Lit::String(_) => PrimTy::String,

            &Lit::Int(_, kind) => return Ok(self.conv_int_kind(kind)),
            &Lit::Float(_, kind) => return Ok(self.conv_float_kind(kind)),
        };

        Ok(self.tyctx_mut().prim(prim))
    }

    fn synth_path(&mut self, path: Path) -> TyResult<Ty> {
        let path = self.hir.path(path);
        self.lookup_typed_term_ty(path.target_name())
            .ok_or_else(|| {
                MessageBuilder::error()
                    .span(path.span())
                    .text(format!("Term {} does not have a type", path))
                    .emit_single_label(self);
                TyError()
            })
    }

    fn synth_ty_expr(&mut self, ty_expr: &TyExpr) -> TyResult<Ty> {
        // FIXME: Check wf?
        // FIXME: Do we need `try_to`?
        self.try_to(|this| {
            let anno_ty = this.conv(ty_expr.ty);
            this.check(ty_expr.expr, anno_ty)
        })
    }

    fn synth_block(&mut self, block: Block) -> TyResult<Ty> {
        let block = self.hir.block(block);

        // Note: Kind of stupid shit
        // FIXME: Move to HIR helper `get_block_decls
        // FIXME: OR replace with stack of block contexts
        // let decls = block
        //     .stmts()
        //     .iter()
        //     .filter_map(|&stmt| match self.hir.stmt(stmt).kind() {
        //         StmtKind::Expr(_) => None,
        //         &StmtKind::Item(item) => match self.hir.item(item).kind() {
        //             ItemKind::TyAlias(_) | ItemKind::Mod(_) => None,
        //             ItemKind::Decl(_) => Some(item),
        //         },
        //     })
        //     .collect::<Vec<ItemId>>();

        self.under_new_ctx(|this| {
            block.stmts().iter().try_for_each(|&stmt| {
                this.synth_stmt(stmt)?;
                Ok(())
            })?;

            let res_ty = block
                .expr()
                .map_or(Ok(this.tyctx_mut().unit()), |&expr| this.synth_expr(expr));

            this.default_number_exes();

            res_ty
        })

        // decls.into_iter().for_each(|item_id| {
        //     let hir_id = item_id.hir_id();
        //     let ty = self.tyctx().node_type(hir_id).unwrap();

        //     if self.tyctx().is_instantiated(ty) {
        //         return;
        //     }

        //     let tys = self.tyctx().ty(ty);

        //     match tys.kind() {
        //         TyKind::Error => todo!(),
        //         TyKind::Var(_) => todo!(),
        //         TyKind::Existential(_) => todo!(),
        //         TyKind::Func(_, _) => todo!(),
        //         TyKind::Forall(_, _) => todo!(),

        //         &TyKind::Prim(PrimTy::IntEx(ex)) => {
        //             let int = self.tyctx_mut().prim(PrimTy::Int(DEFAULT_INT_KIND));
        //             self.solve(ex, int);
        //             self.tyctx_mut().type_node(hir_id, int);
        //         },

        //         &TyKind::Prim(PrimTy::FloatEx(ex)) => {
        //             let float = self.tyctx_mut().prim(PrimTy::Float(DEFAULT_FLOAT_KIND));
        //             self.solve(ex, float);
        //             self.tyctx_mut().type_node(hir_id, float);
        //         },

        //         TyKind::Unit | TyKind::Prim(_) => unreachable!(),
        //     }
        // });
    }

    fn synth_lambda(&mut self, lambda: &Lambda) -> TyResult<Ty> {
        // Parameter is a pattern which can contain multiple name bindings
        // FIXME: Should these existentials be inside context?
        // let param_names = self.hir.pat_names(lambda.param).map_or(vec![], |names| {
        //     names
        //         .iter()
        //         .map(|&name| (name, self.add_fresh_ex()))
        //         .collect::<Vec<_>>()
        // });

        let param_name = match self.hir.pat(lambda.param).kind() {
            &hir::pat::PatKind::Ident(name) => name,
        };
        let param_name_ex = self.add_fresh_common_ex();

        let body_ex = self.add_fresh_common_ex();

        self.under_new_ctx(|this| {
            // FIXME: What if pattern w/o name?
            // param_names.iter().for_each(|(name, name_ex)| {
            //     this.type_term(*name, name_ex.1);
            // });

            this.type_term(param_name, param_name_ex.1);

            let body_ty = this.check(lambda.body, body_ex.1)?;

            let param_ty = this.apply_ctx_on(param_name_ex.1);

            this.tyctx_mut().type_node(lambda.param, param_ty);

            Ok(this.tyctx_mut().func(param_ty, body_ty))
        })
    }

    fn synth_call(&mut self, call: &Call) -> TyResult<Ty> {
        let lhs_ty = self.synth_expr(call.lhs)?;
        let lhs_ty = self.apply_ctx_on(lhs_ty);
        self._synth_call(lhs_ty, call.arg)
    }

    fn _synth_call(&mut self, lhs_ty: Ty, arg: Expr) -> TyResult<Ty> {
        verbose!(
            "Synthesize call {} with arg {}",
            self.tyctx().pp(lhs_ty),
            arg
        );

        match self.tyctx().ty(lhs_ty).kind() {
            // FIXME: Or return Ok(lhs_ty)?
            TyKind::Error => Err(TyError()),
            TyKind::Unit | TyKind::Prim(_) | TyKind::Var(_) => todo!("Non-callable type"),
            &TyKind::Existential(ex) => {
                // // FIXME: Under context or `try_to` to escape types?
                self.try_to(|this| {
                    let param_ex = this.add_fresh_common_ex();
                    let body_ex = this.add_fresh_common_ex();
                    let func_ty = this.tyctx_mut().func(param_ex.1, body_ex.1);
                    this.solve(ex, func_ty);

                    this.check(arg, param_ex.1)?;
                    Ok(body_ex.1)
                })
            },
            &TyKind::Func(param, body) => {
                self.check(arg, param)?;
                Ok(body)
            },
            &TyKind::Forall(alpha, ty) => {
                let alpha_ex = self.add_fresh_common_ex();
                let substituted_ty = self.substitute(ty, Subst::Name(alpha), alpha_ex.1);
                self._synth_call(substituted_ty, arg)
            },
        }
    }

    fn synth_item(&mut self, item: ItemId) -> TyResult<Ty> {
        match self.sess.def_table.get_def(item.def_id()).unwrap().kind() {
            &DefKind::Builtin(bt) => return Ok(self.tyctx().builtin_ty(bt)),
            DefKind::DeclareBuiltin => return Ok(self.tyctx_mut().unit()),
            _ => {},
        }

        let item = self.hir.item(item);

        let ty = match item.kind() {
            ItemKind::TyAlias(_ty) => {
                // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                self.conv_ty_alias(item.def_id());
                self.tyctx_mut().unit()
            },
            ItemKind::Mod(Mod { items }) => {
                for item in items {
                    self.synth_item(*item)?;
                }
                self.tyctx_mut().unit()
            },
            ItemKind::Decl(decl) => {
                let value_ty = self.synth_expr(decl.value)?;
                self.type_term(item.name(), value_ty);

                // Note: Actually, declaration type is a unit type, but we save it
                // TODO: Add encapsulation layer such as `get_def_ty` (with closed access to TyCtx::typed) which will check if definition CAN have a type
                value_ty
            },
        };

        self.tyctx_mut()
            .type_node(HirId::new_owner(item.def_id()), ty);

        TyResult::Ok(ty)
    }

    fn synth_stmt(&mut self, stmt: Stmt) -> TyResult<Ty> {
        let stmt = self.hir.stmt(stmt);

        match stmt.kind() {
            &StmtKind::Expr(expr) => {
                self.synth_expr(expr)?;
            },
            &StmtKind::Item(item) => {
                self.synth_item(item)?;
            },
        }

        Ok(self.tyctx_mut().unit())
    }

    // Check //
    fn check(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        match self._check(expr_id, ty) {
            Ok(ok) => {
                verbose!("[+] Expr {} is of type {}", expr_id, self.tyctx().pp(ty));
                Ok(self.apply_ctx_on(ok))
            },
            Err(err) => {
                verbose!(
                    "[-] Expr {} is NOT of type {}",
                    expr_id,
                    self.tyctx().pp(ty)
                );

                let ty = self.tyctx().ty(ty);

                let span = self.hir.expr_result_span(expr_id);

                MessageBuilder::error()
                    .span(span)
                    .text(format!(
                        "Type mismatch: expected {}{}",
                        ty,
                        if let Some(got) = self.tyctx().node_type(expr_id) {
                            format!(", got {}", got)
                        } else {
                            "".to_string()
                        }
                    ))
                    .label(span, format!("Must be of type {}", ty))
                    .emit(self);

                Err(err)
            },
        }
    }

    fn _check(&mut self, expr_id: Expr, ty: Ty) -> TyResult<Ty> {
        let expr = self.hir.expr(expr_id);
        let tys = self.tyctx().ty(ty);

        match (expr.kind(), tys.kind()) {
            (&ExprKind::Lit(lit), &TyKind::Prim(prim)) => {
                if let Ok(lit_prim) = PrimTy::try_from(lit) {
                    if lit_prim == prim {
                        return Ok(ty);
                    } else {
                        // Unequal literals (not existentials as we not failed
                        //  to construct PrimTy of Lit without context)
                        return Err(TyError());
                    }
                }
            },

            (ExprKind::Lambda(lambda), &TyKind::Func(param_ty, body_ty)) => {
                let param_name = self.hir.pat_names(lambda.param).unwrap();
                assert!(param_name.len() == 1);
                let param_name = param_name[0];

                return self.under_ctx(InferCtx::new_with_term(param_name, param_ty), |this| {
                    this._check(lambda.body, body_ty)
                });
            },

            (_, &TyKind::Forall(alpha, body)) => {
                return self.under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this._check(expr_id, body)?;
                    Ok(ty)
                })
            },

            _ => {},
        }

        let expr_ty = self.synth_expr(expr_id)?;
        let l = self.apply_ctx_on(expr_ty);
        let r = self.apply_ctx_on(ty);

        self.subtype(Spanned::new(expr.span(), l), r)
    }

    // Subtyping //
    fn subtype(&mut self, check_ty: Spanned<Ty>, r_ty: Ty) -> TyResult<Ty> {
        match self._subtype(*check_ty.node(), r_ty) {
            Ok(ty) => {
                verbose!(
                    "[+] {} is a subtype of {}",
                    self.tyctx().pp(*check_ty.node()),
                    self.tyctx().pp(ty)
                );
                Ok(self.apply_ctx_on(ty))
            },
            Err(err) => {
                verbose!(
                    "[-] {} is NOT a subtype of {}",
                    self.tyctx().pp(*check_ty.node()),
                    self.tyctx().pp(r_ty)
                );

                let span = check_ty.span();
                let l_ty = self.tyctx().ty(*check_ty.node());
                let r_ty = self.tyctx().ty(r_ty);

                MessageBuilder::error()
                    .span(span)
                    .text(format!("{} is not a subtype of {}", l_ty, r_ty))
                    .label(span, format!("{} is not a subtype of {}", l_ty, r_ty))
                    .emit(self);

                Err(err)
            },
        }
    }

    // fn prim_subtype(&mut self, l_prim: PrimTy, r_prim: PrimTy) -> TyResult<Ty> {
    //     let eq = match (l_prim, r_prim) {
    //         (PrimTy::Bool, PrimTy::Bool) => true,
    //         (PrimTy::Int(kind), PrimTy::Int(kind_)) => kind == kind_,
    //         (PrimTy::Float(kind), PrimTy::Float(kind_)) => kind == kind_,
    //         (PrimTy::String, PrimTy::String) => true,

    //         _ => false,
    //     };

    //     if eq {
    //         Ok(self.tyctx_mut().prim(r_prim))
    //     } else {
    //         Err(TyError())
    //     }
    // }

    fn _subtype(&mut self, l_ty: Ty, r_ty: Ty) -> TyResult<Ty> {
        assert!(self.ty_wf(l_ty).is_ok());
        assert!(self.ty_wf(r_ty).is_ok());

        match (self.tyctx().ty(l_ty).kind(), self.tyctx().ty(r_ty).kind()) {
            (&TyKind::Prim(prim), &TyKind::Prim(prim_)) if prim == prim_ => Ok(r_ty),

            (TyKind::Var(name1), TyKind::Var(name2)) if name1.sym() == name2.sym() => Ok(r_ty),

            (TyKind::Existential(ex1), TyKind::Existential(ex2)) if ex1 == ex2 => {
                self.ty_wf(l_ty).map(|_| r_ty)
            },

            (&TyKind::Existential(int_ex), TyKind::Prim(PrimTy::Int(_))) if int_ex.is_int() => {
                Ok(self.solve(int_ex, r_ty))
            },

            (TyKind::Existential(int_ex), _) if int_ex.is_int() => Err(TyError()),

            (&TyKind::Existential(float_ex), TyKind::Prim(PrimTy::Float(_)))
                if float_ex.is_float() =>
            {
                Ok(self.solve(float_ex, r_ty))
            },

            (TyKind::Existential(float_ex), _) if float_ex.is_float() => Err(TyError()),

            (&TyKind::Func(param1, body1), &TyKind::Func(param2, body2)) => {
                // self.under_new_ctx(|this| {
                // Enter Θ
                self._subtype(param1, param2)?;
                let body1 = self.apply_ctx_on(body1);
                let body2 = self.apply_ctx_on(body2);
                self._subtype(body1, body2)
                // })
            },

            (&TyKind::Forall(alpha, body), _) => {
                verbose!("forall {}. {} subtype of (?)", alpha, self.tyctx().pp(body));
                let ex = self.fresh_ex(ExistentialKind::Common);
                let ex_ty = self.tyctx_mut().existential(ex);
                let with_substituted_alpha = self.substitute(body, Subst::Name(alpha), ex_ty);

                self.under_ctx(InferCtx::new_with_ex(ex), |this| {
                    this._subtype(with_substituted_alpha, r_ty)
                })
            },

            (_, &TyKind::Forall(alpha, body)) => self
                .under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this._subtype(l_ty, body)
                }),

            (&TyKind::Existential(ex), _) if ex.is_common() => {
                if !self.ty_occurs_in(r_ty, Subst::Existential(ex)) {
                    self.instantiate_l(ex, r_ty)
                } else {
                    todo!("Cycle error");
                    Err(TyError())
                }
            },

            (_, &TyKind::Existential(ex)) if ex.is_common() => {
                if !self.ty_occurs_in(l_ty, Subst::Existential(ex)) {
                    self.instantiate_r(l_ty, ex)
                } else {
                    todo!("Cycle error");
                    Err(TyError())
                }
            },

            _ => Err(TyError()),
        }
    }

    fn try_instantiate_common(&mut self, ex: Existential, ty: Ty) -> TyResult<Ty> {
        let tys = self.tyctx().ty(ty);

        // Inst(L|R)Reach
        match tys.kind() {
            &TyKind::Existential(ty_ex) => {
                let ex_depth = self.find_unbound_ex_depth(ex);
                let ty_ex_depth = self.find_unbound_ex_depth(ty_ex);
                if ex_depth <= ty_ex_depth {
                    let ex_ty = self.tyctx_mut().existential(ex);

                    verbose!(
                        "Instantiate L|R Reach {} = {}",
                        ty_ex,
                        self.tyctx().pp(ex_ty)
                    );

                    self.solve(ty_ex, ex_ty);
                    return Ok(self.apply_ctx_on(ex_ty));
                }
            },
            _ => {},
        }

        // Inst(L|R)Solve
        if self.tyctx().is_mono(ty) {
            verbose!("Instantiate L|R Solve {} = {}", ex, self.tyctx().pp(ty));
            // FIXME: check WF?
            self.solve(ex, ty);
            return Ok(self.apply_ctx_on(ty));
        }

        Err(TyError())
    }

    fn instantiate_l(&mut self, ex: Existential, r_ty: Ty) -> TyResult<Ty> {
        if let Ok(ok) = self.try_instantiate_common(ex, r_ty) {
            return Ok(ok);
        }

        verbose!("Instantiate Left {} = {}", ex, self.tyctx().pp(r_ty));

        let ty = self.tyctx().ty(r_ty);

        match ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Prim(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            &TyKind::Func(param, body) => self.try_to(|this| {
                let range_ex = this.add_fresh_common_ex();
                let domain_ex = this.add_fresh_common_ex();

                let func_ty = this.tyctx_mut().func(domain_ex.1, range_ex.1);

                this.solve(ex, func_ty);

                this.instantiate_r(param, domain_ex.0)?;

                let range_ty = this.apply_ctx_on(body);
                this.instantiate_l(range_ex.0, range_ty)
            }),
            &TyKind::Forall(alpha, body) => self.try_to(|this| {
                this.under_ctx(InferCtx::new_with_var(alpha), |this| {
                    this.instantiate_l(ex, body)
                })
            }),
        }
    }

    fn instantiate_r(&mut self, l_ty: Ty, ex: Existential) -> TyResult<Ty> {
        if let Ok(ok) = self.try_instantiate_common(ex, l_ty) {
            return Ok(ok);
        }

        verbose!("Instantiate Right {} = {}", ex, self.tyctx().pp(l_ty));

        let ty = self.tyctx().ty(l_ty);

        match ty.kind() {
            TyKind::Error
            | TyKind::Unit
            | TyKind::Prim(_)
            | TyKind::Var(_)
            | TyKind::Existential(_) => {
                unreachable!("Unchecked monotype in `instantiate_l`")
            },
            &TyKind::Func(param, body) => self.try_to(|this| {
                let domain_ex = this.add_fresh_common_ex();
                let range_ex = this.add_fresh_common_ex();

                let func_ty = this.tyctx_mut().func(domain_ex.1, range_ex.1);

                this.solve(ex, func_ty);

                this.instantiate_l(domain_ex.0, param)?;

                let range_ty = this.apply_ctx_on(body);
                this.instantiate_r(range_ty, range_ex.0)
            }),
            &TyKind::Forall(alpha, body) => self.try_to(|this| {
                verbose!(
                    "Instantiate forall Right {} = {}",
                    ex,
                    this.tyctx().pp(l_ty)
                );

                let alpha_ex = this.fresh_ex(ExistentialKind::Common);

                this.under_ctx(InferCtx::new_with_ex(alpha_ex), |this| {
                    let alpha_ex_ty = this.tyctx_mut().existential(alpha_ex);
                    let body_ty = this.substitute(body, Subst::Name(alpha), alpha_ex_ty);
                    this.instantiate_r(body_ty, ex)
                })
            }),
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
