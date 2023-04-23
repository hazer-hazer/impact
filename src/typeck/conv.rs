use super::{
    kind::Kind,
    ty::{Adt, ExKind, Field, FloatKind, IntKind, TyVarId, Variant, VariantId},
    tyctx::TyCtx,
    Typecker,
};
use crate::{
    dt::idx::IndexVec,
    hir::{
        self,
        item::{GenericParams, ItemId, TyAlias},
        ty::TyPath,
        visitor::HirVisitor,
        HirId, Path, Res, WithHirId, HIR,
    },
    message::message::{MessageBuilder, MessageHolder, MessageStorage},
    resolve::{
        builtin::Builtin,
        def::{DefId, DefKind, DefMap},
    },
    session::Session,
    span::{
        sym::{Ident, Internable},
        WithSpan,
    },
    typeck::Ty,
};

struct TyConv<'hir> {
    hir: &'hir HIR,

    ty_params: DefMap<TyVarId>,

    msg: MessageStorage,
    sess: Session,
}

impl<'hir> MessageHolder for TyConv<'hir> {
    fn storage(&mut self) -> &mut MessageStorage {
        &mut self.msg
    }
}

impl<'hir> TyConv<'hir> {
    fn tyctx(&self) -> &TyCtx {
        &self.sess.tyctx
    }

    fn tyctx_mut(&mut self) -> &mut TyCtx {
        &mut self.sess.tyctx
    }

    fn conv(&mut self, ty: hir::Ty, ty_def_id: Option<DefId>) -> Ty {
        let ty_node = self.hir.ty(ty);

        match ty_node.kind() {
            hir::ty::TyKind::Path(TyPath(path)) => {
                todo!()
            },
            hir::ty::TyKind::Func(params, body) => {
                let params = params
                    .iter()
                    .copied()
                    .map(|param| self.conv(param, ty_def_id))
                    .collect();
                let body = self.conv(*body, ty_def_id);
                Ty::func(None, params, body)
            },
            hir::ty::TyKind::App(cons, args) => match self.hir.ty(*cons).kind() {
                hir::ty::TyKind::Builtin(bt) => match bt {
                    Builtin::RefTy => {
                        let mut args = args
                            .iter()
                            .map(|&arg| self.conv(arg, ty_def_id))
                            .collect::<Vec<_>>();
                        assert_eq!(args.len(), 1);
                        return Ty::ref_to(args.remove(0));
                    },
                    _ => panic!(),
                },
                _ => todo!(),
            },
            hir::ty::TyKind::Builtin(bt) => match bt {
                Builtin::UnitTy => Ty::unit(),
                Builtin::I32 => Ty::int(IntKind::I32),
                Builtin::Str => Ty::str(),
                Builtin::RefTy => {
                    let kind_var = Kind::next_kind_var_id();
                    Ty::ty_kind(Kind::new_forall(
                        kind_var,
                        Kind::new_abs(
                            Kind::new_var(kind_var),
                            Kind::new_ty(Ty::ref_to(Ty::ty_kind(Kind::new_var(kind_var)))),
                        ),
                    ))
                },
                Builtin::RefCons | Builtin::AddInt | Builtin::SubInt | Builtin::UnitValue => {
                    unreachable!()
                },
            },
        }
    }

    fn conv_ty_path(&mut self, path: Path, ty_def_id: Option<DefId>) -> Ty {
        let path = self.hir.path(path);
        match path.res() {
            &Res::Def(def_kind, def_id) => {
                if let Some(ty_def_id) = ty_def_id {
                    if ty_def_id == def_id {
                        todo!("Recursive type handling")
                    }
                }

                match def_kind {
                    DefKind::TyAlias => {
                        // Path conversion is done linearly, i.e. we get type alias from HIR and
                        // convert its type, caching it
                        // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                        self.conv_ty_alias(def_id)
                    },

                    DefKind::TyParam => self.conv_ty_param(def_id),

                    DefKind::Adt => self.conv_adt(def_id),

                    // // TODO: We need type collector to collect items types before typeck of
                    // expressions
                    DefKind::Variant => {
                        panic!("Variant cannot be used as a type")
                    },

                    // Non-type definitions from type namespace
                    // FIXME: Must be check in resolver and not appear on typeck stage
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
                    | DefKind::Ctor
                    | DefKind::FieldAccessor
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

    fn generalize_ty(
        &mut self,
        generics: &GenericParams,
        mut make_ty: impl FnMut(&mut Self) -> Ty,
    ) -> Ty {
        let ty_params = generics
            .ty_params
            .iter()
            .map(|ty_param| {
                let ty_var_id = Ty::next_ty_var_id();
                assert!(self.ty_params.insert(ty_param.def_id, ty_var_id).is_none());
                ty_var_id
            })
            .collect::<Vec<_>>();
        let ty = make_ty(self);
        ty_params
            .iter()
            .copied()
            .fold(ty, |ty, ty_param| Ty::forall(ty_param, ty))
    }

    fn conv_ty_def(
        &mut self,
        def_id: DefId,
        generics: &GenericParams,
        mut make_ty: impl FnMut(&mut Self) -> Ty,
    ) -> Ty {
        if let Some(def_ty) = self.tyctx().def_ty(def_id) {
            return def_ty;
        }

        let ty = self.generalize_ty(generics, make_ty);

        self.tyctx_mut().type_def(def_id, ty);

        ty
    }

    fn conv_ty_alias(&mut self, ty_alias_def_id: DefId) -> Ty {
        let &TyAlias { ty, ref generics } = self
            .hir
            .item(ItemId::new(ty_alias_def_id.into()))
            .ty_alias();

        self.conv_ty_def(ty_alias_def_id, generics, |this| {
            this.conv(ty, Some(ty_alias_def_id))
        })
    }

    fn conv_ty_param(&mut self, ty_param_def_id: DefId) -> Ty {
        Ty::var(self.ty_params.get_copied_unwrap(ty_param_def_id))
    }

    fn conv_adt(&mut self, adt_def_id: DefId) -> Ty {
        let adt = self.hir.item(ItemId::new(adt_def_id.into())).adt();

        // Variant indexing defined here. For now, just incremental sequencing.
        let variants: IndexVec<VariantId, hir::Variant> = adt.variants.iter().copied().collect();

        let adt_ty = self.conv_ty_def(adt_def_id, &adt.generics, |this| {
            let variants = variants
                .iter()
                .map(|v| this.conv_variant(v, adt_def_id))
                .collect();
            Ty::adt(Adt {
                def_id: adt_def_id,
                variants,
            })
        });

        variants.iter_enumerated().for_each(|(vid, &v_hir_id)| {
            let variant_node = self.hir.variant(v_hir_id);
            let v_def_id = variant_node.def_id;

            self.tyctx_mut().set_variant_id(v_def_id, vid);
            self.tyctx_mut().type_node(v_hir_id.into(), adt_ty);
            self.tyctx_mut().type_def(v_def_id, adt_ty);

            // Constructor type
            let ctor_def_id = variant_node.ctor_def_id;
            let ctor_ty = adt_ty.substituted_forall_body(Ty::func(
                Some(ctor_def_id),
                adt_ty.as_adt().field_tys(vid),
                adt_ty,
            ));
            self.tyctx_mut().type_def(ctor_def_id, ctor_ty)
        });

        adt_ty
    }

    fn conv_variant(&mut self, &variant: &hir::Variant, adt_def_id: DefId) -> Variant {
        let variant = self.hir.variant(variant);
        Variant {
            def_id: variant.def_id,
            name: variant.name,
            fields: variant
                .fields
                .iter()
                .enumerate()
                .map(|(index, field)| self.conv_field(index, field, adt_def_id))
                .collect(),
        }
    }

    fn conv_field(&mut self, index: usize, field: &hir::item::Field, adt_def_id: DefId) -> Field {
        let ty = self.conv(field.ty, Some(adt_def_id));
        self.tyctx_mut().type_node(field.id(), ty);
        Field {
            name: field
                .name
                .unwrap_or_else(|| Ident::new(field.span(), index.to_string().intern())),
            ty,
        }
    }
}

impl<'hir> HirVisitor for TyConv<'hir> {
    fn visit_ty(&mut self, &hir_ty: &hir::Ty, hir: &HIR) {
        self.tyctx_mut().add_conv(hir_ty, self.conv(hir_ty, None));
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, id: ItemId, hir: &HIR) {
        self.tyctx_mut()
            .type_node(id.hir_id(), self.conv_ty_alias(id.def_id()));
    }

    fn visit_adt_item(&mut self, name: Ident, adt: &hir::item::Adt, id: ItemId, hir: &HIR) {
        self.tyctx_mut()
            .type_node(id.hir_id(), self.conv_adt(id.def_id()))
    }
}
