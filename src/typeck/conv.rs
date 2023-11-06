use super::{
    kind::{Kind, KindEx},
    ty::{Adt, Field, FieldId, IntKind, Struct, TyVarId, Variant, VariantData, VariantId},
    tyctx::TyCtx,
};
use crate::{
    dt::idx::IndexVec,
    hir::{
        self,
        item::{GenericParams, ItemId, TyAlias, TyParam},
        visitor::{walk_each, HirVisitor},
        BodyOwner, Map, TyDefKind, TyPath, TyRes, WithHirId, HIR,
    },
    message::message::{impl_message_holder, MessageBuilder, MessageStorage},
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

pub struct TyConv<'hir> {
    hir: &'hir HIR,

    ty_params: DefMap<TyVarId>,

    msg: MessageStorage,
    sess: Session,
}

impl_message_holder!(TyConv<'hir>);
impl_session_holder!(TyConv<'hir>);

impl<'hir> TyConv<'hir> {
    pub fn new(sess: Session, hir: &'hir HIR) -> Self {
        Self {
            hir,
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
        let ty_node = self.hir.ty(ty);

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
            hir::ty::TyKind::App(cons, args) => match self.hir.ty(*cons).kind() {
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
        let path = self.hir.ty_path(path);
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
            .hir
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

        let _degeneralized_adt_ty = adt_ty.degeneralize();

        variants.iter_enumerated().for_each(|(vid, &v_hir_id)| {
            let variant_node = self.hir.variant(v_hir_id);
            let v_def_id = variant_node.def_id;

            self.tyctx_mut().set_variant_id(v_def_id, vid);
            self.tyctx_mut().type_node(v_hir_id, adt_ty);
            self.tyctx_mut().type_def(v_def_id, adt_ty);

            // Constructor type
            // FIXME: replace tight_func with () -> ...
            let ctor_def_id = variant_node.ctor_def_id;
            let ctor_ty = adt_ty.substituted_forall_body(Ty::tight_func(
                Some(ctor_def_id),
                adt_ty
                    .as_adt()
                    .unwrap()
                    .variant(vid)
                    .data
                    .fields
                    .iter()
                    .map(|f| f.ty)
                    .collect(),
                adt_ty,
            ));

            self.tyctx_mut().type_def(ctor_def_id, ctor_ty)
        });

        adt_ty
    }

    fn conv_variant(&mut self, &variant: &hir::Variant, adt_def_id: DefId) -> Variant {
        let variant = self.hir.variant(variant);
        Variant {
            name: variant.name,
            data: self.conv_variant_data(&variant.fields, adt_def_id),
        }
    }

    fn conv_struct(&mut self, struct_def_id: DefId) -> Ty {
        let struct_ = self.hir.item(ItemId::new(struct_def_id.into())).struct_();

        let struct_ty = self.conv_ty_def(struct_def_id, &struct_.generics, |this| {
            Ty::struct_(Struct {
                data: this.conv_variant_data(&struct_.fields, struct_def_id),
            })
        });

        let degeneralized_struct_ty = struct_ty.degeneralize();

        // Field accessors types
        let fields_tys = degeneralized_struct_ty
            .as_struct()
            .unwrap()
            .walk_tys()
            .collect::<IndexVec<FieldId, _>>();

        struct_
            .fields
            .iter()
            .enumerate()
            .for_each(|(index, field)| {
                // Field indexing defined here. For now, just incremental sequencing.
                let field_id = FieldId::new(index as u32);
                let field_ty = fields_tys.get(field_id).copied().unwrap();

                let field_accessor_def_id = field.accessor_def_id.unwrap();
                // Field accessor ty: `AdtTy -> FieldTy`
                let field_accessor_ty = struct_ty.substituted_forall_body(Ty::tight_func(
                    Some(field_accessor_def_id),
                    vec![degeneralized_struct_ty],
                    field_ty,
                ));

                self.tyctx_mut()
                    .type_def(field_accessor_def_id, field_accessor_ty);
                self.tyctx_mut()
                    .set_field_accessor_field_id(field_accessor_def_id, field_id);
            });

        let ctor_ty = struct_ty.substituted_forall_body(Ty::tight_func(
            Some(struct_.ctor_def_id),
            struct_ty
                .as_struct()
                .unwrap()
                .data
                .fields
                .iter()
                .map(|f| f.ty)
                .collect(),
            struct_ty,
        ));

        self.tyctx_mut().type_def(struct_.ctor_def_id, ctor_ty);

        // Check struct if fully named or fully anon
        if let Some(first_field) = struct_.fields.first() {
            assert!(struct_
                .fields
                .iter()
                .fold(first_field.name.is_some(), |named, field| named
                    == field.name.is_some()));
        }

        struct_ty
    }

    fn conv_variant_data(&mut self, fields: &[hir::item::Field], adt_def_id: DefId) -> VariantData {
        let named = if let Some(first_field) = fields.first() {
            let named = first_field.name.is_some();
            let all_same = fields
                .iter()
                .fold(named, |named, field| field.name.is_some() == named);

            // User error must be produced in AST checks
            assert!(all_same);
        };

        VariantData {
            def_id: adt_def_id,
            fields: fields
                .iter()
                .map(|f| Field {
                    name: f.name,
                    ty: self.conv(f.ty, Some(adt_def_id)),
                })
                .collect(),
        }
    }
}

impl<'hir> HirVisitor for TyConv<'hir> {
    fn visit_ty_param(&mut self, ty_param: &TyParam, _hir: &HIR) {
        assert!(self
            .ty_params
            .insert(ty_param.def_id, Ty::next_ty_var_id(Some(ty_param.name)))
            .is_none());
    }

    fn visit_value_item(&mut self, name: Ident, value: hir::BodyId, id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_body(value, BodyOwner::value(id.def_id()), hir);

        let ex = Ty::ty_kind(Kind::new_ex(KindEx::new(self.tyctx_mut().fresh_kind_ex())));
        self.tyctx_mut().type_def(id.def_id(), ex);
    }

    fn visit_func_item(&mut self, name: Ident, body: hir::BodyId, id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_body(body, BodyOwner::func(id.def_id()), hir);

        let ex = Ty::ty_kind(Kind::new_ex(KindEx::new(self.tyctx_mut().fresh_kind_ex())));
        self.tyctx_mut().type_def(id.def_id(), ex);
    }

    fn visit_extern_item(
        &mut self,
        name: Ident,
        extern_item: &hir::item::ExternItem,
        id: ItemId,
        hir: &HIR,
    ) {
        self.visit_ident(name, hir);
        self.visit_ty(extern_item.ty, hir);

        let ty = self.conv(extern_item.ty, None);
        self.tyctx_mut().type_def(id.def_id(), ty);
    }

    fn visit_ty(&mut self, hir_ty: hir::Ty, _hir: &HIR) {
        let conv = self.conv(hir_ty, None);
        self.tyctx_mut().add_conv(hir_ty, conv);
    }

    fn visit_type_item(&mut self, name: Ident, ty_item: &TyAlias, id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_generic_params(&ty_item.generics, hir);
        self.visit_ty(ty_item.ty, hir);

        let conv = self.conv_ty_alias(id.def_id());
        self.tyctx_mut().type_node(id.hir_id(), conv);
    }

    fn visit_adt_item(&mut self, name: Ident, adt: &hir::item::Adt, id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_generic_params(&adt.generics, hir);
        walk_each!(self, adt.variants.iter(), visit_variant, hir);

        let conv = self.conv_adt(id.def_id());
        self.tyctx_mut().type_node(id.hir_id(), conv);
    }

    fn visit_struct_item(&mut self, name: Ident, data: &hir::item::Struct, id: ItemId, hir: &HIR) {
        self.visit_ident(name, hir);
        self.visit_generic_params(&data.generics, hir);
        walk_each!(self, data.fields.iter(), visit_field, hir);

        let conv = self.conv_struct(id.def_id());
        self.tyctx_mut().type_node(id.hir_id(), conv);
    }
}

impl<'hir> Stage<()> for TyConv<'hir> {
    fn run(mut self) -> StageResult<(), Session> {
        self.visit_hir(self.hir);
        stage_result(self.sess, (), self.msg)
    }
}
