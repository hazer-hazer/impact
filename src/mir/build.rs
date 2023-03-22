use std::collections::{hash_map::Entry, HashMap};

use crate::{
    hir::{self, visitor::HirVisitor, BodyId, BodyOwnerKind, HirId, OwnerId, HIR},
    message::message::MessageStorage,
    session::{Session, Stage, StageOutput},
    typeck::tyctx::TyCtx,
};

use super::{
    thir::{build::ThirBuilder, ExprId, LocalVar, ParamId, THIR},
    BBWith, Body, BodyBuilder, LValue, Local, MIR,
};

macro_rules! unpack {
    ($bb_name: ident = $bb_with: expr) => {{
        let BBWith(bb_, with) = $bb_with;
        $bb_name = bb_;
        with
    }};

    ($bb_with: expr) => {{
        let BBWith(bb, ()) = $bb_with;
        bb
    }};
}

pub(super) use unpack;

pub(super) struct MirBuilder<'ctx> {
    thir_entry_expr: ExprId,
    pub thir: THIR,
    pub builder: BodyBuilder,
    pub bindings: HashMap<LocalVar, Local>,

    hir: &'ctx HIR,
    pub sess: &'ctx Session,
}

impl<'ctx> MirBuilder<'ctx> {
    pub fn build(body_owner: OwnerId, hir: &'ctx HIR, sess: &'ctx Session) -> Body {
        let (thir, thir_entry_expr) =
            ThirBuilder::new(hir, &sess.tyctx, body_owner).build_body_thir();

        let this = Self {
            thir_entry_expr,
            thir,
            builder: BodyBuilder::default(),
            bindings: Default::default(),
            hir,
            sess,
        };

        match this.hir.body_owner_kind(this.thir.body_owner()) {
            BodyOwnerKind::Func | BodyOwnerKind::Lambda => this.func(),
            // TODO: Review `Value`s are globals with once-called initializer functions.
            BodyOwnerKind::Value => this.func(),
        }
    }

    fn func(mut self) -> Body {
        let bb = self.builder.begin_bb();

        self.push_return_local(
            self.sess
                .tyctx
                .tyof(HirId::new_owner(self.thir.body_owner().into()))
                .return_ty(),
            self.hir.return_ty_span(self.thir.body_owner().into()),
        );

        // Note: Only one param for now
        [ParamId::new(0)].into_iter().for_each(|param| {
            self.declare_param_bindings(param);
        });

        let bb = unpack!(self.store_expr(bb, LValue::return_lvalue(), self.thir_entry_expr));

        self.builder.terminate_return(bb);

        self.builder.emit()
    }
}

pub struct BuildFullMir<'ctx> {
    hir: &'ctx HIR,

    mir: MIR,

    msg: MessageStorage,
    sess: Session,
}

impl<'ctx> BuildFullMir<'ctx> {
    pub fn new(sess: Session, hir: &'ctx HIR) -> Self {
        Self {
            sess,
            hir,
            mir: Default::default(),
            msg: Default::default(),
        }
    }
}

impl<'ctx> BuildFullMir<'ctx> {
    fn build(&mut self, body_id: BodyId, body_owner: OwnerId) {
        match self.mir.bodies.entry(body_id) {
            Entry::Occupied(_) => {},
            Entry::Vacant(entry) => {
                let body = MirBuilder::build(body_owner, self.hir, &self.sess);
                entry.insert(body);
            },
        }
    }
}

impl<'ctx> HirVisitor for BuildFullMir<'ctx> {
    fn visit_func_item(
        &mut self,
        _name: crate::span::span::Ident,
        body: &hir::BodyId,
        id: hir::item::ItemId,
        _hir: &HIR,
    ) {
        if id.def_id() == self.sess.def_table.builtin_func().def_id() {
            return;
        }
        self.build(*body, id.into());
    }

    fn visit_lambda(&mut self, lambda: &hir::expr::Lambda, _hir: &HIR) {
        self.build(lambda.body_id, lambda.def_id.into());
    }
}

impl<'ctx> Stage<MIR> for BuildFullMir<'ctx> {
    fn run(mut self) -> StageOutput<MIR> {
        self.visit_hir(self.hir);
        StageOutput::new(self.sess, self.mir, self.msg)
    }
}
