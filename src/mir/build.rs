use std::collections::HashMap;

use crate::{
    dt::idx::Idx,
    hir::{BodyOwnerKind, HirId, OwnerId, HIR},
    mir::thir::PatKind,
    typeck::tyctx::TyCtx,
};

use super::{
    thir::{build::ThirBuilder, ExprId, LocalVar, ParamId, Pat, THIR},
    Body, BodyBuilder, LValue, Local, LocalInfo, START_BB,
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
    tyctx: &'ctx TyCtx,
}

impl<'ctx> MirBuilder<'ctx> {
    pub fn build(body_owner: OwnerId, hir: &'ctx HIR, tyctx: &'ctx TyCtx) -> Body {
        let (thir, thir_entry_expr) = ThirBuilder::new(hir, tyctx, body_owner).build_body_thir();

        let this = Self {
            thir_entry_expr,
            thir,
            builder: BodyBuilder::default(),
            bindings: Default::default(),
            hir,
            tyctx,
        };

        match this.hir.body_owner_kind(this.thir.body_owner()) {
            BodyOwnerKind::Func | BodyOwnerKind::Lambda => this.func(),
        }
    }

    fn func(mut self) -> Body {
        self.push_return_local(
            self.tyctx
                .tyof(HirId::new_owner(self.thir.body_owner().into()))
                .return_ty(),
            // AGENDA
        );

        // Note: Only one param for now
        [ParamId::new(0)].into_iter().for_each(|param| {
            self.declare_param_bindings(param);
        });

        self.store_expr(START_BB, LValue::return_lvalue(), self.thir_entry_expr);

        self.builder.emit()
    }
}
