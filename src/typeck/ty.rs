use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    ast::expr::Lit,
    session::Session,
    span::span::{Ident, Symbol},
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type {
    kind: Rc<TypeKind>,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self {
            kind: Rc::new(kind),
        }
    }

    pub fn kind(&self) -> &TypeKind {
        &self.kind
    }

    /*
     * [τ/α]A
     * Where:
     * - τ is `to` existential
     * - α is a type variable name `from`
     * - and Α is our type (`self`)
     */
    pub fn instantiate(&self, from: Ident, to: ExistentialId) -> Type {
        match *self.kind() {
            TypeKind::Var(id) if id == from => Type::new(TypeKind::Existential(to)),

            TypeKind::Unit | TypeKind::Lit(_) | TypeKind::Var(_) => self.clone(),

            TypeKind::Func(ref a, ref b) => Type::new(TypeKind::Func(
                a.instantiate(from, to),
                b.instantiate(from, to),
            )),

            TypeKind::Forall(id, ref ty) => {
                if id == from {
                    self.clone()
                } else {
                    Type::new(TypeKind::Forall(id, ty.instantiate(from, to)))
                }
            }

            TypeKind::Existential(id) => {
                assert!(id != to);
                self.clone()
            }
        }
    }

    /**
     * Check if type references existential variable id
     */
    pub fn occurs_in(&self, id: ExistentialId) -> bool {
        match *self.kind() {
            TypeKind::Unit | TypeKind::Lit(_) | TypeKind::Var(_) => false,
            TypeKind::Func(ref a, ref b) => a.occurs_in(id) || b.occurs_in(id),
            TypeKind::Forall(_, ref ty) => ty.occurs_in(id),
            TypeKind::Existential(id1) => id == id1,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LitTy {
    Bool,
    Int,
    String,
}

impl LitTy {
    pub fn from_lit_expr(lit: Lit) -> Self {
        match lit {
            Lit::Bool(_) => Self::Bool,
            Lit::Int(_) => Self::Int,
            Lit::String(_) => Self::String,
        }
    }
}

impl Display for LitTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LitTy::Bool => "bool",
                LitTy::Int => "int",
                LitTy::String => "string",
            }
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeKind {
    Unit,
    Lit(LitTy),
    Var(Ident),
    Func(Type, Type),
    Forall(Ident, Type),
    Existential(ExistentialId),
}

#[derive(Clone, Eq, PartialEq)]
pub enum CtxItem {
    TypeDecl(Ident),
    VarType(Ident, Type),
    ExistentialDecl(ExistentialId, Option<Type>),
    Marker(ExistentialId),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ExistentialId(pub u32);

#[derive(Clone)]
pub struct Ctx {
    pub items: Vec<CtxItem>,
    pub existentials: u32,
}

pub struct TypeError(String);
pub type TypeResult<T> = Result<T, TypeError>;

impl TypeError {
    pub fn new(msg: String) -> Self {
        Self(msg)
    }
}

impl Ctx {
    pub fn initial() -> Self {
        Self {
            items: vec![],
            existentials: 0,
        }
    }

    pub fn extended(&self, item: CtxItem) -> Ctx {
        let mut items = self.items.clone();
        let mut existentials = self.existentials.clone();

        match item {
            CtxItem::ExistentialDecl(id, _) => {
                existentials = std::cmp::max(id.0 + 1, existentials);
            }
            _ => {}
        }

        items.push(item);

        Self { items, existentials }
    }

    pub fn with_item<R, F>(&mut self, item: CtxItem, body: F) -> R
    where
        F: FnOnce(&mut Ctx) -> R,
    {
        self.items.push(item);
        let result = body(self);
        self.items.pop();
        result
    }

    pub fn slice_to(&self, length: usize) -> Ctx {
        Ctx {
            items: self.items[..length].to_owned(),
            existentials: self.existentials,
        }
    }

    pub fn any<F>(&self, pred: F) -> bool
    where
        F: FnMut(&CtxItem) -> bool,
    {
        self.items.iter().any(pred)
    }

    pub fn contains(&self, item: CtxItem) -> bool {
        self.any(|it| *it == item)
    }

    pub fn lookup_existential(&self, id: ExistentialId) -> Option<Type> {
        self.items
            .iter()
            .filter_map(|item| match *item {
                CtxItem::ExistentialDecl(id1, ref ty) if id1 == id => Some(ty.clone()),
                _ => None,
            })
            .next()
            .unwrap()
    }

    pub fn lookup_var(&self, id: Ident) -> Option<Type> {
        self.items
            .iter()
            .filter_map(|item| match *item {
                CtxItem::VarType(id1, ref ty) if id == id1 => Some(ty.clone()),
                _ => None,
            })
            .next()
    }

    /**
     * Type fell-formedness rules
     */
    pub fn ty_wf(&mut self, ty: &Type) -> TypeResult<()> {
        match *ty.kind() {
            /*
             * ——————— UnitWF
             *  Γ ⊢ 1
             */
            TypeKind::Unit | TypeKind::Lit(_) => Ok(()),

            /*
             * —————————— UvarWF
             *  Γ[α] ⊢ α
             */
            TypeKind::Var(id) => {
                if self.contains(CtxItem::TypeDecl(id)) {
                    Ok(())
                } else {
                    Err(TypeError(format!(
                        "Type {:?} is ill-formed (type variable undeclared under the context)",
                        ty
                    )))
                }
            }

            /*
             *  Γ ⊢ Α  Γ ⊢ Β
             * —————————————— ArrowWF
             *   Γ ⊢ Α → Β
             */
            TypeKind::Func(ref a, ref b) => {
                self.ty_wf(a)?;
                self.ty_wf(b)
            }

            /*
             *  Γ, α ⊢ Α
             * —————————— ForallWF
             * Γ ⊢ ∀α. Α
             */
            TypeKind::Forall(id, ref ty) => {
                self.with_item(CtxItem::TypeDecl(id), |ctx| ctx.ty_wf(ty))
            }

            /*
             * ——————————— EvarWF
             * Γ[α^] ⊢ α^
             */
            TypeKind::Existential(id) => {
                if self.find_existential_decl(id).is_some() {
                    Ok(())
                } else {
                    Err(TypeError(format!(
                        "type {:?} is ill-formed (existential undeclared under the context)",
                        ty
                    )))
                }
            }
        }
    }

    pub fn find_existential_decl(&self, id: ExistentialId) -> Option<(usize, Option<Type>)> {
        self.items
            .iter()
            .enumerate()
            .filter_map(|(i, item)| match *item {
                CtxItem::ExistentialDecl(id1, ref fv) if id == id1 => Some((i, fv.clone())),
                _ => None,
            })
            .next()
    }

    pub fn substitute(&self, ty: &Type) -> Type {
        match *ty.kind() {
            TypeKind::Unit | TypeKind::Lit(_) | TypeKind::Var(_) => ty.clone(),
            TypeKind::Func(ref a, ref b) => {
                Type::new(TypeKind::Func(self.substitute(a), self.substitute(b)))
            }
            TypeKind::Forall(id, ref ty) => Type::new(TypeKind::Forall(id, self.substitute(ty))),
            TypeKind::Existential(id) => match self.lookup_existential(id) {
                Some(u) => self.substitute(&u),
                None => ty.clone(),
            },
        }
    }

    pub fn try_under_context<F, T>(&mut self, op: F) -> TypeResult<T>
    where
        F: FnOnce(&mut Ctx) -> TypeResult<T>,
    {
        let preserved: Ctx = self.clone();
        match op(self) {
            Ok(v) => Ok(v),
            Err(e) => {
                *self = preserved;
                Err(e)
            }
        }
    }

    pub fn subtype(&mut self, a_ty: &Type, b_ty: &Type) -> TypeResult<()> {
        match (a_ty.kind(), b_ty.kind()) {
            /*
             * —————————————————————— <:Var
             *  Γ[α] ⊢ α <: α ⊣ Γ[α]
             */
            (&TypeKind::Var(a_id), &TypeKind::Var(b_id)) if a_id == b_id => self.ty_wf(a_ty),

            /*
             * ——————————————— <:Unit
             * Γ ⊢ 1 <: 1 ⊣ Γ
             */
            (&TypeKind::Unit, &TypeKind::Unit) | (&TypeKind::Lit(_), &TypeKind::Lit(_)) => Ok(()),

            /*
             * —————————————————— <: Exvar
             * Γ[α^] ⊢ α^ ⊣ Γ[α^]
             */
            (&TypeKind::Existential(a_id), &TypeKind::Existential(b_id)) if a_id == b_id => {
                self.ty_wf(a_ty)
            }

            /*
             * Γ ⊢ Β₁ <: Α₁ ⊣ Θ  Θ ⊢ [Θ]Α₂ <: [Θ]Β₂ ⊣ Δ
             * ————————————————————————————————————————— <:→
             *       Γ ⊢ Α₁ → Α₂ <: Β₁ → Β₂ ⊣ Δ
             */
            (&TypeKind::Func(ref a_in, ref a_out), &TypeKind::Func(ref b_in, ref b_out)) => self
                .try_under_context(|this| {
                    this.subtype(b_in, a_in)?;
                    let a_out1 = this.substitute(a_out);
                    let b_out1 = this.substitute(b_out);
                    this.subtype(&a_out1, &b_out1)
                }),

            /*
             * Γ, ‣α^, α^ ⊢ [α^/α]Α <: B ⊣ Δ, ‣α^, Θ
             * ————————————————————————————————————— <:∀L
             *          Γ ⊢ ∀α. Α <: Β ⊣ Δ
             */
            (&TypeKind::Forall(alpha_id, ref a_ty), _) => self.try_under_context(|this| {
                let alpha_ex_id = this.fresh_existential();
                this.items.push(CtxItem::Marker(alpha_ex_id));
                this.items.push(CtxItem::ExistentialDecl(alpha_ex_id, None));

                let a_ty = a_ty.instantiate(alpha_id, alpha_ex_id);
                this.subtype(&a_ty, b_ty)?;
                this.pop_marker(alpha_ex_id)
            }),

            /*
             * Γ, α ⊢ Α <: Β ⊣ Δ, α, Θ
             * ———————————————————————— <:∀R
             *   Γ ⊢ Α <: ∀α. Β ⊣ Δ
             */
            (_, &TypeKind::Forall(alpha_id, ref b_ty)) => self.try_under_context(|this| {
                this.items.push(CtxItem::TypeDecl(alpha_id));
                this.subtype(a_ty, b_ty)?;
                this.pop_type_decl(alpha_id)
            }),

            /*
             * α^ ∉ FV(Α)  Γ[α^] ⊢ α^ :=≤ Α ⊣ Δ
             * ————————————————————————————————— InstantiateL
             *    Γ[α^] ⊢ α^ <: Α ⊣ Δ
             *
             * I use Α :=≤ α for "instantiate α^ to a subtype of Α"
             */
            (&TypeKind::Existential(a_id), _) => self.try_under_context(|this| {
                if !b_ty.occurs_in(a_id) {
                    this.ty_wf(a_ty)?;
                    this.instantiate_left(a_id, b_ty)
                } else {
                    Err(TypeError(format!(
                        "subtype({:?}, {:?}) -- cycle on {:?}",
                        a_ty, b_ty, a_id
                    )))
                }
            }),

            /*
             * α^ ∉ FV(Α)  Γ[α^] ⊢ Α :=≤ α^ ⊣ Δ
             * ————————————————————————————————— <:InstantiateR
             *      Γ[α^] ⊢ Α <: α^ ⊣ Δ
             */
            (_, &TypeKind::Existential(b_id)) => self.try_under_context(|this| {
                if !a_ty.occurs_in(b_id) {
                    this.ty_wf(b_ty)?;
                    this.instance_right(a_ty, b_id)
                } else {
                    Err(TypeError(format!(
                        "subtype({:?}, {:?}) -- cycle on {:?}",
                        a_ty, b_ty, b_id
                    )))
                }
            }),

            _ => Err(TypeError(format!("subtype: no match"))),
        }
    }

    pub fn instantiate_left(&mut self, alpha_id: ExistentialId, b_ty: &Type) -> TypeResult<()> {
        let alpha_index = self.find_unbound_existential(alpha_id)?;

        if let &TypeKind::Existential(beta_id) = b_ty.kind() {
            let beta_index = self.find_unbound_existential(beta_id)?;
            if alpha_index < beta_index {
                return self.assign(
                    beta_index,
                    beta_id,
                    &Type::new(TypeKind::Existential(alpha_id)),
                );
            }
        }

        match *b_ty.kind() {
            TypeKind::Lit(_) => todo!(),

            TypeKind::Existential(_) | TypeKind::Var(_) | TypeKind::Unit => {
                self.slice_to(alpha_index).ty_wf(b_ty)?;
                self.assign(alpha_index, alpha_id, b_ty)
            }

            TypeKind::Forall(id, ref ty) => self.try_under_context(|this| {
                this.items.push(CtxItem::TypeDecl(id));
                this.instantiate_left(alpha_id, ty);
                this.pop_type_decl(id)
            }),

            TypeKind::Func(ref domain_ty, ref range_ty) => self.try_under_context(|this| {
                let domain_id = this.fresh_existential();
                let range_id = this.fresh_existential();

                this.assign(
                    alpha_index,
                    alpha_id,
                    &Type::new(TypeKind::Func(
                        Type::new(TypeKind::Existential(domain_id)),
                        Type::new(TypeKind::Existential(range_id)),
                    )),
                );

                this.items
                    .insert(alpha_index, CtxItem::ExistentialDecl(domain_id, None));
                this.items
                    .insert(alpha_index, CtxItem::ExistentialDecl(range_id, None));

                this.instance_right(domain_ty, domain_id)?;

                let range_ty = this.substitute(range_ty);
                this.instantiate_left(range_id, &range_ty)
            }),
        }
    }

    pub fn instance_right(&mut self, a_ty: &Type, alpha_id: ExistentialId) -> TypeResult<()> {
        let alpha_index = self.find_unbound_existential(alpha_id)?;

        if let &TypeKind::Existential(beta_id) = a_ty.kind() {
            let beta_index = self.find_unbound_existential(beta_id)?;
            if alpha_index < beta_index {
                return self.assign(
                    beta_index,
                    beta_id,
                    &Type::new(TypeKind::Existential(alpha_id)),
                );
            }
        }

        match *a_ty.kind() {
            TypeKind::Lit(_) => todo!(),

            TypeKind::Existential(_) | TypeKind::Var(_) | TypeKind::Unit => {
                self.slice_to(alpha_index).ty_wf(a_ty)?;
                self.assign(alpha_index, alpha_id, a_ty)
            }

            TypeKind::Forall(beta_id, ref b_ty) => self.try_under_context(|this| {
                let beta_ex_id = this.fresh_existential();
                this.items.push(CtxItem::Marker(beta_ex_id));
                this.items.push(CtxItem::ExistentialDecl(beta_ex_id, None));

                let b_ty = b_ty.instantiate(beta_id, beta_ex_id);
                this.instance_right(&b_ty, alpha_id)?;
                this.pop_marker(beta_ex_id)
            }),

            TypeKind::Func(ref domain_ty, ref range_ty) => self.try_under_context(|this| {
                let domain_id = this.fresh_existential();
                let range_id = this.fresh_existential();

                this.assign(
                    alpha_index,
                    alpha_id,
                    &Type::new(TypeKind::Func(
                        Type::new(TypeKind::Existential(domain_id)),
                        Type::new(TypeKind::Existential(range_id)),
                    )),
                );

                this.items
                    .insert(alpha_index, CtxItem::ExistentialDecl(domain_id, None));
                this.items
                    .insert(alpha_index, CtxItem::ExistentialDecl(range_id, None));

                this.instantiate_left(domain_id, domain_ty)?;

                let range_ty = this.substitute(range_ty);
                this.instance_right(&range_ty, range_id)
            }),
        }
    }

    pub fn fresh_existential(&mut self) -> ExistentialId {
        let id = self.existentials;
        self.existentials += 1;
        ExistentialId(id)
    }

    pub fn find_unbound_existential(&self, alpha: ExistentialId) -> TypeResult<usize> {
        match self.find_existential_decl(alpha) {
            Some((idx, None)) => Ok(idx),
            Some((idx, Some(_))) => Err(TypeError(format!(
                "find_unbound_existential({:?}) -- already bound",
                alpha
            ))),
            None => Err(TypeError(format!(
                "find_unbound_existential({:?}) -- not in scope",
                alpha
            ))),
        }
    }

    pub fn pop_marker(&mut self, eid: ExistentialId) -> TypeResult<()> {
        while let Some(item) = self.items.pop() {
            match item {
                CtxItem::Marker(id) if id == eid => {
                    return Ok(());
                }
                _ => {}
            }
        }

        assert!(false, "marker for {:?} not found", eid);
        Err(TypeError(format!("pop_marker({:?})", eid)))
    }

    pub fn pop_type_decl(&mut self, id: Ident) -> TypeResult<()> {
        while let Some(item) = self.items.pop() {
            match item {
                CtxItem::TypeDecl(id1) if id1 == id => {
                    return Ok(());
                }
                _ => {}
            }
        }

        assert!(false, "type decl for {:?} not found", id);
        Err(TypeError(format!("pop_type_decl({:?})", id)))
    }

    pub fn assign(&mut self, index: usize, id: ExistentialId, ty: &Type) -> TypeResult<()> {
        match &mut self.items[index] {
            &mut CtxItem::ExistentialDecl(id1, ref mut v) => {
                assert_eq!(id, id1);
                assert!(v.is_none());
                *v = Some(ty.clone());
                Ok(())
            }
            _ => {
                assert!(false);
                Err(TypeError(format!(
                    "assign({:?}, {:?}, {:?})",
                    index, id, ty
                )))
            }
        }
    }
}
