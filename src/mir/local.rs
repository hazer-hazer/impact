use crate::span::span::{Ident, Internable, Span};

use super::{
    build::MirBuilder,
    thir::{LocalVar, ParamId, Pat, PatKind},
    LValue, Local, LocalInfo, Ty,
};

impl<'ctx> MirBuilder<'ctx> {
    fn bind_local_var(&mut self, var: LocalVar, local: Local) {
        assert!(self.bindings.insert(var, local).is_none());
    }

    pub(super) fn resolve_local_var(&mut self, var: LocalVar) -> Local {
        self.bindings.get(&var).copied().unwrap()
    }

    pub(super) fn push_return_local(&mut self, ty: Ty, span: Span) {
        assert_eq!(
            self.builder.push_local(
                false,
                LocalInfo {
                    ty,
                    name: Ident::new(span, "return_local".intern()),
                    span,
                    user_defined: false,
                }
            ),
            Local::return_local()
        );
    }

    pub(super) fn declare_param_bindings(&mut self, param_id: ParamId) {
        if param_id == 0 {
            assert!(self.builder.locals.len() == 1);
        }

        let pat = self.thir.param(param_id).pat;
        self.declare_bindings(&pat, true);
    }

    pub fn declare_bindings(&mut self, pat: &Pat, is_param: bool) {
        match pat.kind {
            PatKind::Unit => {},
            PatKind::Ident { var, ty, name } => {
                let local = self.builder.push_local(
                    is_param,
                    LocalInfo {
                        ty,
                        name,
                        user_defined: true,
                        span: pat.span,
                    },
                );
                self.bind_local_var(var.into(), local);
            },
        }
    }

    pub(super) fn temp_lvalue(&mut self, ty: Ty, span: Span) -> LValue {
        let local_sym = self.builder.next_local_name().intern();
        self.builder
            .push_local(
                false,
                LocalInfo {
                    ty,
                    name: Ident::new(span, local_sym),
                    span,
                    user_defined: false,
                },
            )
            .lvalue()
    }
}
