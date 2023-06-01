use super::{
    kind::{Kind, KindEx},
    ty::{Adt, Field, FieldId, IntKind, TyVarId, Variant, VariantId},
    tyctx::TyCtx,
};
use crate::{
    dt::idx::IndexVec,
    hir::{
        self,
        item::{GenericParams, ItemId, TyAlias, TyParam},
        visitor::{walk_each, HirVisitor},
        BodyOwner, TyDefKind, TyPath, TyRes, WithHirId, HIR,
    },
    message::message::{impl_message_holder, MessageHolder, MessageStorage},
    resolve::{
        builtin::TyBuiltin,
        def::{DefId, DefMap},
    },
    session::{impl_session_holder, stage_result, Session, SessionHolder, Stage, StageResult},
    span::{
        sym::{Ident, Internable},
        WithSpan,
    },
    typeck::Ty,
};

pub struct TyConv {
    ty_params: DefMap<TyVarId>,

    msg: MessageStorage,
    sess: Session,
}

impl_message_holder!(TyConv);
impl_session_holder!(TyConv);

impl TyConv {
    pub fn new(sess: Session) -> Self {
        Self {
            ty_params: Default::default(),
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

    fn conv(&mut self, ty: hir::Ty, ty_def_id: Option<DefId>) -> Ty {
        let ty_node = self.hir().ty(ty);

        match ty_node.kind() {
            &hir::ty::TyKind::Path(path) => self.conv_ty_path(path, ty_def_id),
            hir::ty::TyKind::Func(params, body) => {
                let params = params
                    .iter()
                    .copied()
                    .map(|param| self.conv(param, ty_def_id))
                    .collect();
                let body = self.conv(*body, ty_def_id);
                Ty::func(None, params, body)
            },
            hir::ty::TyKind::App(cons, args) => match self.hir().ty(*cons).kind() {
                hir::ty::TyKind::Builtin(bt) => match bt {
                    TyBuiltin::RefTy => {
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
                TyBuiltin::UnitTy => Ty::unit(),
                TyBuiltin::I32 => Ty::int(IntKind::I32),
                TyBuiltin::Str => Ty::str(),
                TyBuiltin::RefTy => {
                    let kind_var = Kind::next_kind_var_id(None);
                    Ty::ty_kind(Kind::new_forall(
                        kind_var,
                        Kind::new_abs(
                            Kind::new_var(kind_var),
                            Kind::new_ty(Ty::ref_to(Ty::ty_kind(Kind::new_var(kind_var)))),
                        ),
                    ))
                },
            },
        }
    }

    fn conv_ty_path(&mut self, path: TyPath, ty_def_id: Option<DefId>) -> Ty {
        let path = self.hir().ty_path(path);
        match path.res() {
            &TyRes::Def(def_kind, def_id) => {
                if let Some(ty_def_id) = ty_def_id {
                    if ty_def_id == def_id {
                        todo!("Recursive type handling")
                    }
                }

                match def_kind {
                    TyDefKind::TyAlias => {
                        // Path conversion is done linearly, i.e. we get type alias from HIR and
                        // convert its type, caching it
                        // FIXME: Type alias item gotten two times: one here, one in `conv_ty_alias`
                        self.conv_ty_alias(def_id)
                    },

                    TyDefKind::TyParam => self.conv_ty_param(def_id),

                    TyDefKind::Adt => self.conv_adt(def_id),
                }
            },
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
            .map(|ty_param| self.ty_params.get_copied_unwrap(ty_param.def_id))
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
        make_ty: impl FnMut(&mut Self) -> Ty,
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
            .hir()
            .item(ItemId::new(ty_alias_def_id.into()))
            .ty_alias();

        self.conv_ty_def(ty_alias_def_id, generics, |this| {
            this.conv(ty, Some(ty_alias_def_id))
        })
    }

    fn conv_ty_param(&mut self, ty_param_def_id: DefId) -> Ty {
        Ty::var(
            self.ty_params
                .get_flat(ty_param_def_id)
                .copied()
                .expect(&format!(
                    "Type parameter {ty_param_def_id} declaration not found"
                )),
        )
    }

    fn conv_adt(&mut self, adt_def_id: DefId) -> Ty {
        let adt = self.hir().item(ItemId::new(adt_def_id.into())).adt();

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

        let degeneralized_adt_ty = adt_ty.degeneralize();

        variants.iter_enumerated().for_each(|(vid, &v_hir_id)| {
            let variant_node = self.hir().variant(v_hir_id);
            let v_def_id = variant_node.def_id;

            self.tyctx_mut().set_variant_id(v_def_id, vid);
            self.tyctx_mut().type_node(v_hir_id, adt_ty);
            self.tyctx_mut().type_def(v_def_id, adt_ty);

            let fields_tys = degeneralized_adt_ty.as_adt().unwrap().field_tys(vid);

            // Constructor type
            // FIXME: replace tight_func with () -> ...
            let ctor_def_id = variant_node.ctor_def_id;
            let ctor_ty = adt_ty.substituted_forall_body(Ty::tight_func(
                Some(ctor_def_id),
                fields_tys.iter().copied().collect(),
                adt_ty,
            ));

            self.tyctx_mut().type_def(ctor_def_id, ctor_ty)
        });

        if !adt.is_adt {
            // Field accessors types
            let variant_node = self.hir().variant(variants[0.into()]);
            let fields_tys = degeneralized_adt_ty.as_adt().unwrap().field_tys(0.into());
            variant_node
                .fields
                .iter()
                .enumerate()
                .for_each(|(index, field)| {
                    // Field indexing defined here. For now, just incremental sequencing.
                    let field_id = FieldId::new(index as u32);
                    let field_ty = fields_tys.get(field_id).copied().unwrap();

                    let field_accessor_def_id = field.accessor_def_id.unwrap();
                    // Field accessor ty: `AdtTy -> FieldTy`
                    let field_accessor_ty = adt_ty.substituted_forall_body(Ty::tight_func(
                        Some(field_accessor_def_id),
                        vec![degeneralized_adt_ty],
                        field_ty,
                    ));

                    self.tyctx_mut()
                        .type_def(field_accessor_def_id, field_accessor_ty);
                    self.tyctx_mut()
                        .set_field_accessor_field_id(field_accessor_def_id, field_id);
                });
        }

        adt_ty
    }

    fn conv_variant(&mut self, &variant: &hir::Variant, adt_def_id: DefId) -> Variant {
        let variant = self.hir().variant(variant);
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

impl HirVisitor for TyConv {
    fn visit_ty_param(&mut self, ty_param: &TyParam) {
        assert!(self
            .ty_params
            .insert(ty_param.def_id, Ty::next_ty_var_id(Some(ty_param.name)))
            .is_none());
    }

    fn visit_value_item(&mut self, name: Ident, value: &hir::BodyId, id: ItemId) {
        self.visit_ident(&name);
        self.visit_body(value, BodyOwner::value(id.def_id()));

        let ex = Ty::ty_kind(Kind::new_ex(KindEx::new(self.tyctx_mut().fresh_kind_ex())));
        self.tyctx_mut().type_def(id.def_id(), ex);
    }

    fn visit_func_item(&mut self, name: Ident, body: &hir::BodyId, id: ItemId) {
        self.visit_ident(&name);
        self.visit_body(body, BodyOwner::func(id.def_id()));

        let ex = Ty::ty_kind(Kind::new_ex(KindEx::new(self.tyctx_mut().fresh_kind_ex())));
        self.tyctx_mut().type_def(id.def_id(), ex);
    }

    fn visit_extern_item(&mut self, name: Ident, extern_item: &hir::item::ExternItem, id: ItemId) {
        self.visit_ident(&name);
        self.visit_ty(&extern_item.ty);

        let ty = self.conv(extern_item.ty, None);
        self.tyctx_mut().type_def(id.def_id(), ty);
    }

    fn visit_ty(&mut self, &hir_ty: &hir::Ty) {
        let conv = self.conv(hir_ty, None);
        self.tyctx_mut().add_conv(hir_ty, conv);
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, id: ItemId) {
        self.visit_ident(&name);
        self.visit_generic_params(&ty_item.generics);
        self.visit_ty(&ty_item.ty);

        let conv = self.conv_ty_alias(id.def_id());
        self.tyctx_mut().type_node(id.hir_id(), conv);
    }

    fn visit_adt_item(&mut self, name: Ident, adt: &hir::item::Adt, id: ItemId) {
        self.visit_ident(&name);
        self.visit_generic_params(&adt.generics);
        walk_each!(self, adt.variants, visit_variant);

        let conv = self.conv_adt(id.def_id());
        self.tyctx_mut().type_node(id.hir_id(), conv);
    }
}

impl Stage<()> for TyConv {
    fn run(mut self) -> StageResult<(), Session> {
        self.visit_hir();
        stage_result(self.sess, (), self.msg)
    }
}
