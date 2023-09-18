use std::fmt::Display;

use super::{
    impl_with_node_id, pr_display, prs_display_join, ty::TyPath, IdentNode, NodeId, WithNodeId, N,
    PR,
};
use crate::span::{impl_with_span, sym::Ident, Span, WithSpan};

#[derive(Debug)]
pub struct StructPatField {
    pub name: Option<PR<Ident>>,
    pub pat: PR<N<Pat>>,
}

impl Display for StructPatField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.name.as_ref() {
            write!(f, "{}: {}", pr_display(name), pr_display(&self.pat))
        } else {
            write!(f, "{}", pr_display(&self.pat))
        }
    }
}

#[derive(Debug)]
pub enum PatKind {
    // TODO: Lit

    // TODO: Replace Unit variant with Path pattern which refers to builtin type `()`
    Unit,
    Ident(PR<IdentNode>),

    Struct(
        TyPath,
        Vec<PR<StructPatField>>,
        /// rest
        bool,
    ),

    Or(PR<N<Pat>>, PR<N<Pat>>),
    Tuple(Vec<PR<N<Pat>>>),
}

impl Display for PatKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatKind::Unit => "()".fmt(f),
            PatKind::Ident(ident) => pr_display(ident).fmt(f),
            PatKind::Struct(struct_path, fields, rest) => write!(
                f,
                "{} {}{}",
                pr_display(&struct_path.0),
                fields.iter().map(pr_display).collect::<Vec<_>>().join(", "),
                if *rest { ", ..." } else { "" }
            ),
            PatKind::Or(lpat, rpat) => write!(f, "{} | {}", pr_display(lpat), pr_display(rpat)),
            PatKind::Tuple(pats) => write!(f, "({})", prs_display_join(pats, ", ")),
        }
    }
}

#[derive(Debug)]
pub struct Pat {
    id: NodeId,
    kind: PatKind,
    span: Span,
}

impl_with_span!(Pat);
impl_with_node_id!(Pat);

impl Pat {
    pub fn new(id: NodeId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &PatKind {
        &self.kind
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}
