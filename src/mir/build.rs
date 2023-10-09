use std::collections::{hash_map::Entry, HashMap};

use super::{
    thir::{build::ThirBuilder, ExprId, LocalVar, ParamId, THIR},
    BBWith, Body, BodyBuilder, LValue, Local, MIR,
};
use crate::{
    cli::{color::WithColor, verbose},
    hir::{self, visitor::HirVisitor, BodyId, BodyOwnerKind, HirId, Map, OwnerId, HIR},
    message::message::{impl_message_holder, MessageStorage},
    pp::{
        pp::PP,
        thir::{ThirPPCtx, ThirPrinter},
    },
    resolve::builtin::Builtin,
    session::{impl_session_holder, stage_result, Session, Stage, StageResult},
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
    pub hir: &'ctx HIR,
    pub thir: THIR,
    pub builder: BodyBuilder,
    pub bindings: HashMap<LocalVar, Local>,

    pub sess: &'ctx Session,
}

impl_session_holder!(MirBuilder<'ctx>);

impl<'ctx> MirBuilder<'ctx> {
    fn should_be_built(sess: &'ctx Session, body_owner: OwnerId) -> bool {
        match sess.def_table.as_builtin(body_owner.into()) {
            Some(bt) => match bt {
                Builtin::RefCons => false,
                _ => true,
            },
            None => true,
        }
    }

    pub fn build(body_owner: OwnerId, hir: &'ctx HIR, sess: &'ctx Session) -> Option<Body> {
        if !Self::should_be_built(sess, body_owner) {
            return None;
        }

        let (thir, thir_entry_expr) = ThirBuilder::new(&sess, hir, body_owner).build_body_thir();

        if true {
            let pp = PP::new(ThirPPCtx::new(&thir));
            verbose!(
                "{} body THIR:\n{}",
                body_owner.colorized(),
                pp.pp(thir_entry_expr).get_string()
            );
        }

        let this = Self {
            thir_entry_expr,
            hir,
            thir,
            builder: BodyBuilder::default(),
            bindings: Default::default(),
            sess,
        };

        Some(match this.hir.body_owner_kind(this.thir.body_owner()) {
            BodyOwnerKind::Func | BodyOwnerKind::Lambda => this.func(),
            // TODO: Review `Value`s are globals with once-called initializer functions.
            BodyOwnerKind::Value => this.func(),
        })
    }

    fn func(mut self) -> Body {
        let bb = self.builder.begin_bb();

        self.push_return_local(
            self.sess
                .tyctx
                .tyof(HirId::new_owner(self.thir.body_owner().into()))
                .body_return_ty(),
            self.hir.body_return_ty_span(self.thir.body_owner().into()),
        );

        // Note: Only one param for now
        (0..self.thir.params_count()).into_iter().for_each(|param| {
            self.declare_param_bindings(ParamId::new(param as u32));
        });

        let bb = unpack!(self.store_expr(bb, LValue::return_lvalue(), self.thir_entry_expr));

        self.builder.terminate_return(bb);

        self.builder.emit()
    }
}

pub struct BuildFullMir<'hir> {
    hir: &'hir HIR,
    mir: MIR,

    msg: MessageStorage,
    sess: Session,
}

impl_message_holder!(BuildFullMir<'ctx>);
impl_session_holder!(BuildFullMir<'ctx>);

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
    fn build(&mut self, body_id: BodyId, hir: &HIR, body_owner: OwnerId) {
        match self.mir.bodies.entry(body_id) {
            Entry::Occupied(_) => {},
            Entry::Vacant(entry) => {
                if let Some(body) = MirBuilder::build(body_owner, hir, &self.sess) {
                    entry.insert(body);
                }
            },
        }
    }
}

impl<'ctx> HirVisitor for BuildFullMir<'ctx> {
    fn visit_func_item(
        &mut self,
        _name: crate::span::sym::Ident,
        body: hir::BodyId,
        id: hir::item::ItemId,
        hir: &HIR,
    ) {
        if id.def_id() == self.sess.def_table.builtin_func().def_id() {
            return;
        }
        self.build(body, hir, id.into());
    }

    fn visit_lambda(&mut self, lambda: &hir::expr::Lambda, hir: &HIR) {
        self.build(lambda.body_id, hir, lambda.def_id.into());
    }

    fn visit_value_item(
        &mut self,
        _name: crate::span::sym::Ident,
        value: BodyId,
        id: hir::item::ItemId,
        hir: &HIR,
    ) {
        self.build(value, hir, id.def_id().into());
    }
}

impl<'ctx> Stage<MIR> for BuildFullMir<'ctx> {
    fn run(mut self) -> StageResult<MIR> {
        self.visit_hir(self.hir);
        stage_result(self.sess, self.mir, self.msg)
    }
}
