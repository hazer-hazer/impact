use std::fmt::Display;

use crate::span::span::{Span, WithSpan};

use super::{
    expr::Expr, pr_display, pr_node_kind_str, NodeId, NodeKindStr, Path, WithNodeId, N, PR,
};

#[derive(Debug)]
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
pub struct TyPath(pub PR<Path>);

#[derive(Debug)]
pub enum TyKind {
    Path(TyPath),
    Func(PR<N<Ty>>, PR<N<Ty>>),
    Paren(PR<N<Ty>>),
    App(PR<N<Ty>>, PR<N<Ty>>),

    // Now only used for builtins but may be used for const parameters
    AppExpr(PR<N<Ty>>, PR<N<Expr>>),
}

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TyKind::Path(path) => write!(f, "{}", pr_display(&path.0)),
            TyKind::Func(param_ty, return_ty) => {
                write!(f, "{} -> {}", pr_display(param_ty), pr_display(return_ty))
            },
            TyKind::Paren(inner) => write!(f, "{}", pr_display(inner)),
            TyKind::App(cons, arg) => write!(f, "{} {}", pr_display(cons), pr_display(arg)),
            TyKind::AppExpr(cons, const_arg) => {
                write!(f, "{} {}", pr_display(cons), pr_display(const_arg))
            },
        }
    }
}

impl NodeKindStr for TyKind {
    fn kind_str(&self) -> String {
        match &self {
            TyKind::Path(path) => format!("type {}", pr_display(&path.0)),
            TyKind::Func(_, _) => "function type".to_string(),

            // I just thought this format would look funny 😐
            TyKind::Paren(inner) => format!("{{{}}}", pr_node_kind_str(inner)),
            TyKind::AppExpr(cons, _) | TyKind::App(cons, _) => {
                format!("{} type constructor", pr_node_kind_str(cons))
            },
        }
    }
}
