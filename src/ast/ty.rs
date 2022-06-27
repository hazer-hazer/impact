use std::fmt::Display;

use crate::{
    hir::N,
    span::span::{Ident, Span, WithSpan},
};

use super::{pr_display, pr_node_kind_str, NodeId, NodeKindStr, PR};

#[derive(Clone, Copy)]
pub enum LitTy {
    Bool,
    Int,
    String,
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

pub struct Ty {
    id: NodeId,
    kind: TyKind,
    span: Span,
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

impl Ty {
    pub fn new(id: NodeId, kind: TyKind, span: Span) -> Self {
        Self { id, kind, span }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }

    pub fn id(&self) -> NodeId {
        self.id
    }
}

pub enum TyKind {
    Unit,
    Lit(LitTy),
    Var(PR<Ident>),
    Func(PR<N<Ty>>, PR<N<Ty>>),
    Paren(PR<N<Ty>>),
}

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyKind::Unit => write!(f, "()"),
            TyKind::Lit(lit_ty) => write!(f, "{}", lit_ty),
            TyKind::Var(ident) => write!(f, "{}", pr_display(ident)),
            TyKind::Func(param_ty, return_ty) => {
                write!(f, "{} -> {}", pr_display(param_ty), pr_display(return_ty))
            }
            TyKind::Paren(inner) => write!(f, "{}", pr_display(inner)),
        }
    }
}

impl NodeKindStr for TyKind {
    fn kind_str(&self) -> String {
        match self {
            TyKind::Unit => "unit type".to_string(),
            TyKind::Lit(_) => "literal type".to_string(),
            TyKind::Var(ident) => format!("type {}", pr_display(ident)),
            TyKind::Func(_, _) => "function type".to_string(),

            // I just thought this format would look funny 😐
            TyKind::Paren(inner) => format!("{{{}}}", pr_node_kind_str(inner)),
        }
    }
}
