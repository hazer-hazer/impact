use std::{
    collections::{
        hash_map::{DefaultHasher, Entry},
        HashMap, HashSet,
    },
    fmt::Display,
    hash::{Hash, Hasher},
};

use super::{
    builtin::builtins,
    ctx::GlobalCtx,
    kind::{KindEx, KindExId},
    ty::{Ex, ExId, FieldId, MapTy, Ty, TyKind, TyMap, TyVarId, VariantId},
};
use crate::{
    cli::verbose,
    dt::idx::IndexVec,
    hir::{self, Expr, HirId},
    resolve::{
        builtin::Builtin,
        def::{DefId, DefMap},
    },
    session::MaybeWithSession,
    span::sym::Ident,
};

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub struct TyBindings(IndexVec<TyVarId, Option<Ty>>);

impl TyBindings {
    fn substitution_for(&self, var: TyVarId) -> Option<Ty> {
        self.0.get_flat(var).copied()
    }

    fn bind(&mut self, var: TyVarId, ty: Ty) -> Option<Ty> {
        self.0.insert(var, ty)
    }

    pub fn iter(&self) -> impl Iterator<Item = (TyVarId, &Ty)> + '_ {
        self.0.iter_enumerated_flat()
    }
}

pub enum InstantiatedTy<T = Ty, E = ()> {
    /// Type does not have instances because is never used
    None,
    /// Monomorphic function instance
    Mono(T),
    /// Polymorphic function instance. List of results of instances with T
    /// (almost always a type) and callee expression id
    Poly(Vec<Result<(T, Expr), E>>),
}

pub struct TyCtx {
    builtins: HashMap<Builtin, Ty>,

    converted: HashMap<hir::Ty, Ty>,

    def_tys: DefMap<Ty>,

    ex: ExId,
    kind_ex: KindExId,

    /// Types associated to DefId's (meaning for each item is different!)
    /// and types of HIR expressions.
    /// - Type alias: `[Type alias DefId] -> [Its type]`
    /// - Declaration: `[Declaration DefId] -> [Type of assigned value]`
    typed: HashMap<HirId, Ty>,

    expr_ty_bindings: HashMap<Expr, TyBindings>,

    def_ty_bindings: DefMap<HashSet<Expr>>,

    variant_indices: DefMap<VariantId>,
    field_accessor_field_id: DefMap<FieldId>,

    // Metadata //
    // FIXME: Never set
    ty_names: TyMap<Ident>,
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            builtins: builtins(),
            converted: Default::default(),
            def_tys: Default::default(),
            ex: ExId::new(0),
            kind_ex: KindExId::new(0),
            typed: Default::default(),
            expr_ty_bindings: Default::default(),
            def_ty_bindings: Default::default(),
            variant_indices: Default::default(),
            field_accessor_field_id: Default::default(),
            ty_names: Default::default(),
        }
    }

    pub fn add_conv(&mut self, hir: hir::Ty, ty: Ty) {
        assert!(self.converted.insert(hir, ty).is_none());
    }

    pub fn get_conv(&self, hir: hir::Ty) -> Option<Ty> {
        self.converted.get(&hir).copied()
    }

    pub fn type_def(&mut self, def_id: DefId, ty: Ty) {
        assert!(self.def_tys.insert(def_id, ty).is_none());
    }

    pub fn def_ty(&self, def_id: DefId) -> Option<Ty> {
        self.def_tys.get_flat(def_id).copied()
    }

    pub fn def_ty_exes(&self) -> Vec<(DefId, Ex)> {
        self.def_tys
            .iter_enumerated_flat()
            .filter_map(|(def_id, ty)| ty.as_ex().map(|ex| (def_id, ex)))
            .collect()
    }

    pub fn def_ty_kind_exes(&self) -> Vec<(DefId, KindEx)> {
        self.def_tys
            .iter_enumerated_flat()
            .filter_map(|(def_id, ty)| ty.as_kind_ex().map(|ex| (def_id, ex)))
            .collect()
    }

    pub fn fresh_ex(&mut self) -> ExId {
        *self.ex.inc()
    }

    pub fn fresh_kind_ex(&mut self) -> KindExId {
        *self.kind_ex.inc()
    }

    pub fn type_node<Id>(&mut self, id: Id, ty: Ty)
    where
        Id: Into<HirId>,
    {
        let id = id.into();
        // assert!(
        //     self.typed.insert(id, ty).is_none(),
        //     "{} is already typed with {}",
        //     id,
        //     ty
        // );
        // assert!(ty.is_solved());
        match self.typed.entry(id) {
            Entry::Occupied(_) => {},
            Entry::Vacant(vacant) => {
                vacant.insert(ty);
            },
        }
    }

    pub fn apply_ctx_on_typed_nodes(&mut self, ctx: &GlobalCtx) {
        self.typed.iter_mut().for_each(|(_, ty)| {
            let applied = ty.apply_ctx(ctx);
            // TODO: Error reporting instead of assertion
            // FIXME: We need somehow emit messages and then check for bugs like this
            // assert!(applied.is_solved(), "Unsolved type {applied} after typeck");
            if applied != *ty {
                verbose!(
                    "Applying global ctx on ty it is solved as {} => {}",
                    (*ty).without_sess(),
                    applied.without_sess()
                );
            }
            *ty = applied;
        });
    }

    pub fn builtin(&self, bt: Builtin) -> Ty {
        self.builtins.get(&bt).copied().unwrap()
    }

    pub fn node_type<Id>(&self, id: Id) -> Option<Ty>
    where
        Id: Into<HirId>,
    {
        self.typed.get(&id.into()).copied()
    }

    pub fn ty_name(&self, ty: Ty) -> Option<Ident> {
        self.ty_names.get_flat(ty.id()).copied()
    }

    pub fn set_variant_id(&mut self, def_id: DefId, id: VariantId) {
        assert!(self.variant_indices.insert(def_id, id).is_none());
    }

    pub fn variant_id(&self, def_id: DefId) -> VariantId {
        self.variant_indices.get_copied_unwrap(def_id)
    }

    pub fn set_field_accessor_field_id(&mut self, field_accessor_def_id: DefId, id: FieldId) {
        assert!(self
            .field_accessor_field_id
            .insert(field_accessor_def_id, id)
            .is_none());
    }

    pub fn field_accessor_field_id(&self, field_accessor_def_id: DefId) -> FieldId {
        self.field_accessor_field_id
            .get_copied_unwrap(field_accessor_def_id)
    }

    /// Unwrap-version of `node_type` with a short name.
    pub fn tyof<Id>(&self, id: Id) -> Ty
    where
        Id: Into<HirId> + Display + Copy,
    {
        self.node_type(id)
            .expect(&format!("Type of node {} expected", id))
    }

    fn get_binding(&self, expr: Expr, var: TyVarId) -> Option<Ty> {
        self.expr_ty_bindings.get(&expr)?.substitution_for(var)
    }

    pub fn instantiated_expr_ty(&self, expr: Expr) -> Result<Ty, ()> {
        self._instantiated_ty(expr, self.tyof(expr))
    }

    fn _instantiated_ty(&self, expr: Expr, ty: Ty) -> Result<Ty, ()> {
        match ty.kind() {
            TyKind::Error => todo!(),
            TyKind::Unit | TyKind::Bool | TyKind::Int(_) | TyKind::Float(_) | TyKind::Str => Ok(ty),
            &TyKind::Var(var) => self.instantiated_expr_ty_var(expr, var),
            // FIXME: Panic?
            TyKind::Existential(_) => panic!(),
            TyKind::Func(params, body) | TyKind::FuncDef(_, params, body) => Ok(Ty::func(
                ty.func_def_id(),
                params
                    .iter()
                    .copied()
                    .map(|param| self._instantiated_ty(expr, param))
                    .collect::<Result<_, _>>()?,
                self._instantiated_ty(expr, *body)?,
            )),
            &TyKind::Forall(alpha, body) => {
                let alpha_ty = self.instantiated_expr_ty_var(expr, alpha)?;
                Ok(body.substitute(alpha, alpha_ty))
            },
            &TyKind::Ref(inner) => self._instantiated_ty(expr, inner),
            TyKind::Kind(_) => todo!(),
            TyKind::Adt(adt) => Ok(Ty::adt(
                adt.map_ty(&mut |ty| self._instantiated_ty(expr, ty))?,
            )),
            TyKind::Struct(data) => Ok(Ty::struct_(
                data.map_ty(&mut |ty| self._instantiated_ty(expr, ty))?,
            )),
        }
    }

    fn instantiated_expr_ty_var(&self, expr: Expr, var: TyVarId) -> Result<Ty, ()> {
        match self.get_binding(expr, var) {
            Some(ty) => {
                assert!(ty.is_mono());
                Ok(ty)
            },
            // TODO: Type variable name is a number, add namings!
            None => Err(()),
        }
    }

    pub fn instantiated_ty(&self, ty: Ty, def_id: DefId) -> InstantiatedTy {
        if ty.is_instantiated() {
            InstantiatedTy::Mono(ty.mono_checked())
        } else {
            if let Some(bindings) = self.def_ty_bindings.get_flat(def_id) {
                InstantiatedTy::Poly(
                    bindings
                        .iter()
                        .map(|&expr| self.instantiated_expr_ty(expr).map(|ty| (ty, expr)))
                        .collect(),
                )
            } else {
                InstantiatedTy::None
            }
        }
    }

    pub fn bind_ty_var(&mut self, def_id: Option<DefId>, expr: Expr, var: TyVarId, ty: Ty) {
        assert!(
            ty.is_mono(),
            "Cannot bind polytype {} to type variable {}",
            ty.without_sess(),
            var
        );
        assert!(
            self.expr_ty_bindings
                .entry(expr)
                .or_default()
                .bind(var, ty)
                .is_none(),
            "Tried to rebind type variable {} in expression {} to {}",
            var,
            expr,
            ty.without_sess()
        );

        if let Some(def_id) = def_id {
            assert!(self.def_ty_bindings.upsert_default(def_id).insert(expr));
        }
    }

    pub fn expr_ty_bindings(&self) -> &HashMap<Expr, TyBindings> {
        &self.expr_ty_bindings
    }

    /// Get list of expressions with unique types bound in definition.
    /// I.e., having function `id :: forall x. x` and two calls `id 1` and `id
    /// 2`, we'll get list with expression id of one of this calls, because
    /// `x` is bound only to some int. Then, we can get substitutions for
    /// this definitions from `expr_ty_bindings`. We have `Expr -> (TyVarId
    /// -> Ty)[]` and `DefId -> set Expr` mapping. The result must be a list
    /// of expressions with unique substitutions of definition type variables.
    pub fn unique_def_bound_usages(&self, id: DefId) -> Vec<Expr> {
        let mut unique_bindings = HashSet::<u64>::new();
        self.def_ty_bindings
            .get_unwrap(id)
            .iter()
            .fold(Vec::<Expr>::new(), |mut exprs, expr| {
                let mut s = DefaultHasher::new();
                self.expr_ty_bindings.get(expr).unwrap().hash(&mut s);
                if unique_bindings.insert(s.finish()) {
                    exprs.push(*expr);
                }
                exprs
            })
    }

    // Debug //
    pub fn pp_typed_node(&self, id: HirId) -> String {
        self.node_type(id)
            .map_or("(?)".to_string(), |ty| ty.without_sess().to_string())
    }
}
