use super::{super::ty::TyKind, Kind, KindEx, KindSort, Ty};
use crate::{
    typeck::{ctx::InferCtx, TyResult, TypeckErr, Typecker, debug::InferStepKind},
    utils::macros::match_expected,
};

impl<'hir> Typecker<'hir> {
    pub fn subtype_kind(&mut self, l_ty: Ty, r_ty: Ty) -> TyResult<Ty> {
        match (l_ty.kind(), r_ty.kind()) {
            // Two kinds checks below
            (TyKind::Kind(_), TyKind::Kind(_)) => {
                let l_kind = match_expected!(l_ty.kind(), &TyKind::Kind(kind) => kind);
                let r_kind = match_expected!(r_ty.kind(), &TyKind::Kind(kind) => kind);
                self._subtype_kind(l_kind, r_kind)
                    .map(|kind| Ty::ty_kind(kind))
            },

            (&TyKind::Kind(l_kind), _) => match l_kind.sort() {
                &KindSort::Ty(l_ty) => self._subtype(l_ty, r_ty),
                _ => Ok(Ty::ty_kind(self._subtype_kind(l_kind, Kind::new_ty(r_ty))?)),
            },

            (_, &TyKind::Kind(r_kind)) => match r_kind.sort() {
                &KindSort::Ty(r_ty) => self._subtype(l_ty, r_ty),
                _ => Ok(Ty::ty_kind(self._subtype_kind(Kind::new_ty(l_ty), r_kind)?)),
            },

            _ => Err(TypeckErr::LateReport),
        }
    }

    /// Subtype of kinds
    pub fn _subtype_kind(&mut self, l_kind: Kind, r_kind: Kind) -> TyResult<Kind> {
        let subkind = match (l_kind.sort(), r_kind.sort()) {
            (&KindSort::Ty(_), &KindSort::Ty(_)) => unreachable!("Type subtyping goes first"),

            (&KindSort::Var(var), &KindSort::Var(var_)) if var == var_ => Ok(r_kind),

            // TODO: Well-formedness check?
            (&KindSort::Ex(ex), &KindSort::Ex(ex_)) if ex == ex_ => Ok(r_kind),

            (&KindSort::Abs(param1, body1), &KindSort::Abs(param2, body2)) => {
                let param = self._subtype_kind(param1, param2)?;
                let body1 = body1.apply_ctx(self.ctx()).into();
                let body2 = body2.apply_ctx(self.ctx()).into();
                let body = self.subtype_kind(body1, body2)?.expect_kind();
                Ok(Kind::new_abs(param, body))
            },

            (&KindSort::Forall(var, body), _) => {
                let ex = self.fresh_kind_ex();
                let ex_kind = Kind::new_ex(ex);
                let with_var_substituted = body.substitute(var, ex_kind);

                self.under_ctx(InferCtx::new_with_kind_ex(ex), |this| {
                    this._subtype_kind(with_var_substituted, r_kind)
                })
            },

            (_, &KindSort::Forall(var, body)) => self
                .under_ctx(InferCtx::new_with_kind_var(var), |this| {
                    this._subtype_kind(l_kind, body)
                }),

            (&KindSort::Ex(ex), _) => {
                if !r_kind.contains_ex(ex) {
                    self.instantiate_kind_l(ex, r_kind)
                } else {
                    todo!("Kind cycle error")
                }
            },

            (_, &KindSort::Ex(ex)) => {
                if !l_kind.contains_ex(ex) {
                    self.instantiate_kind_r(l_kind, ex)
                } else {
                    todo!("Kind cycle error")
                }
            },

            _ => Err(TypeckErr::LateReport),
        };

        self.dbg
            .step(InferStepKind::SubtypeKind(
                l_kind, r_kind, subkind,
            ));

        subkind
    }

    fn instantiate_kind_l(&mut self, ex: KindEx, r_kind: Kind) -> TyResult<Kind> {
        if let &KindSort::Ex(beta_ex) = r_kind.sort() {
            if self.find_unbound_kind_ex_depth(ex) < self.find_unbound_kind_ex_depth(beta_ex) {
                return Ok(self.solve_kind_ex(beta_ex, Kind::new_ex(ex).mono()));
            }
        }

        match r_kind.sort() {
            KindSort::Ty(_) | KindSort::Var(_) | KindSort::Ex(_) => {
                Ok(self.solve_kind_ex(ex, r_kind.mono()))
            },
            &KindSort::Abs(param, body) => self.try_to(|this| {
                let param_ex = this.fresh_kind_ex();
                let body_ex = this.fresh_kind_ex();

                let ex_abs_kind = Kind::new_abs(Kind::new_ex(param_ex), Kind::new_ex(body_ex));

                this.solve_kind_ex(ex, ex_abs_kind.mono());

                this.instantiate_kind_r(param, param_ex)?;

                let body_kind = body.apply_ctx(this.ctx());
                this.instantiate_kind_l(param_ex, body_kind)
            }),
            &KindSort::Forall(var, body) => self.try_to(|this| {
                this.under_ctx(InferCtx::new_with_kind_var(var), |this| {
                    this.instantiate_kind_l(ex, body)
                })
            }),
        }
    }

    fn instantiate_kind_r(&mut self, l_kind: Kind, ex: KindEx) -> TyResult<Kind> {
        if let &KindSort::Ex(beta_ex) = l_kind.sort() {
            if self.find_unbound_kind_ex_depth(ex) < self.find_unbound_kind_ex_depth(beta_ex) {
                return Ok(self.solve_kind_ex(beta_ex, Kind::new_ex(ex).mono()));
            }
        }

        match l_kind.sort() {
            KindSort::Ty(_) | KindSort::Var(_) | KindSort::Ex(_) => {
                Ok(self.solve_kind_ex(ex, l_kind.mono()))
            },
            &KindSort::Abs(param, body) => self.try_to(|this| {
                let param_ex = this.fresh_kind_ex();
                let body_ex = this.fresh_kind_ex();

                let ex_abs_kind = Kind::new_abs(Kind::new_ex(param_ex), Kind::new_ex(body_ex));

                this.solve_kind_ex(ex, ex_abs_kind.mono());

                this.instantiate_kind_l(ex, param)?;

                let body_kind = body.apply_ctx(this.ctx());
                this.instantiate_kind_r(body_kind, body_ex)
            }),
            &KindSort::Forall(var, body) => self.try_to(|this| {
                let var_ex = this.fresh_kind_ex();

                this.under_ctx(InferCtx::new_with_kind_ex(var_ex), |this| {
                    let var_ex_kind = Kind::new_ex(var_ex);
                    let body_kind = body.substitute(var, var_ex_kind);
                    this.instantiate_kind_r(body_kind, ex)
                })
            }),
        }
    }
}
