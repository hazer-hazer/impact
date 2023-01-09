use std::fmt::Display;

use crate::span::span::{Span, WithSpan};

use super::{pr_display, pr_node_kind_str, NodeId, NodeKindStr, Path, WithNodeId, N, PR};

#[derive(Debug, Clone)]
pub struct Ty {
    id: NodeId,
    kind: TyKind,
    span: Span,
}

impl Ty {
    pub fn new(id: NodeId, kind: TyKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }
}

impl WithNodeId for Ty {
    fn id(&self) -> NodeId {
        self.id
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl WithSpan for Ty {
    fn span(&self) -> Span {
        self.span
    }
}

impl NodeKindStr for Ty {
    fn kind_str(&self) -> String {
        self.kind().kind_str()
    }
}

#[derive(Debug, Clone)]
pub enum TyKind {
    Unit,
    Path(PR<Path>),
    Func(PR<N<Ty>>, PR<N<Ty>>),
    Paren(PR<N<Ty>>),
}

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TyKind::Unit => write!(f, "()"),
            TyKind::Path(path) => write!(f, "{}", pr_display(path)),
            TyKind::Func(param_ty, return_ty) => {
                write!(f, "{} -> {}", pr_display(param_ty), pr_display(return_ty))
            },
            TyKind::Paren(inner) => write!(f, "{}", pr_display(inner)),
        }
    }
}

impl NodeKindStr for TyKind {
    fn kind_str(&self) -> String {
        match &self {
            TyKind::Unit => "unit type".to_string(),
            TyKind::Path(path) => format!("type {}", pr_display(path)),
            TyKind::Func(_, _) => "function type".to_string(),

            // I just thought this format would look funny 😐
            TyKind::Paren(inner) => format!("{{{}}}", pr_node_kind_str(inner)),
        }
    }
}
