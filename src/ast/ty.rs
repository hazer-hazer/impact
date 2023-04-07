use std::fmt::Display;

use crate::span::span::{Span, WithSpan, impl_with_span};

use super::{
    expr::Expr, pr_display, pr_node_kind_str, prs_display_join, NodeId, NodeKindStr, Path,
    WithNodeId, N, PR,
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

impl_with_span!(Ty);

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
    Func(Vec<PR<N<Ty>>>, PR<N<Ty>>),
    Paren(PR<N<Ty>>),
    App(PR<N<Ty>>, Vec<PR<N<Ty>>>),

    // Now only used for builtins but may be used for const parameters
    AppExpr(PR<N<Ty>>, Vec<PR<N<Expr>>>),
}

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TyKind::Path(path) => write!(f, "{}", pr_display(&path.0)),
            TyKind::Func(params, body) => {
                write!(
                    f,
                    "{} -> {}",
                    prs_display_join(params, " "),
                    pr_display(body)
                )
            },
            TyKind::Paren(inner) => write!(f, "{}", pr_display(inner)),
            TyKind::App(cons, args) => {
                write!(f, "{} {}", pr_display(cons), prs_display_join(args, " "))
            },
            TyKind::AppExpr(cons, args) => {
                write!(f, "{} {}", pr_display(cons), prs_display_join(args, " "))
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
