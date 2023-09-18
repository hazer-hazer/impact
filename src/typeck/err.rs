use super::{kind::KindSort, ty::TyKind, Ty, Typecker};
use crate::{
    cli::color::Colorize,
    hir::{HirId, Map},
    message::message::MessageBuilder,
};

impl<'hir> Typecker<'hir> {
    pub fn must_be_inferred(&self, id: HirId) -> Option<MessageBuilder> {
        let ty = self.tyctx().node_type(id);
        if let Some(ty) = ty {
            if ty.is_solved() {
                return None;
            }

            // Do not emit an error if type contains error type.
            // When creating error type some error already must be created.
            if ty.contains_error() {
                return None;
            }
        }

        let ty_str = ty.map_or(Self::uninferred_ty(), |ty| self.ty_str(ty));

        let node = self.hir.node(id);
        let kind = node.kind();
        let span = node.span();

        Some(
            MessageBuilder::error()
                .span(span)
                .text(format!("Failed to infer type of {kind}"))
                .label(span, format!("{ty_str}"))
                // FIXME: Add and use `Debug` labels
                .label(span, format!("[DEBUG] {}", ty.map_or(Self::uninferred_ty(), |ty| ty.to_string()))),
        )
    }

    // FIXME: Replace with `Ty::pretty_str` and `Kind::pretty_str`
    fn ty_str(&self, ty: Ty) -> String {
        match ty.kind() {
            TyKind::Existential(_) => Self::uninferred_ty(),
            TyKind::Kind(kind) => match kind.sort() {
                KindSort::Ex(_) => Self::uninferred_ty(),
                _ => kind.to_string(),
            },
            _ => ty.to_string(),
        }
    }

    fn uninferred_ty() -> String {
        format!("{}", "(?)".blue())
    }
}
