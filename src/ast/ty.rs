use std::fmt::Display;

use super::{
    expr::Expr, is_block_ended, pr_display, pr_node_kind_str, prs_display_join, IsBlockEnded,
    NodeId, NodeKindStr, Path, WithNodeId, N, PR,
};
use crate::span::{impl_with_span, Span, WithSpan};

#[derive(Debug)]
pub struct Ty {
    id: NodeId,
    kind: TyKind,
    span: Span,
    is_block_ended: bool,
}

impl Ty {
    pub fn new(id: NodeId, kind: TyKind, span: Span, is_block_ended: bool) -> Self {
        Self {
            id,
            kind,
            span,
            is_block_ended,
        }
    }

    pub fn kind(&self) -> &TyKind {
        &self.kind
    }
}

impl IsBlockEnded for Ty {
    fn is_block_ended(&self) -> bool {
        self.is_block_ended
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
            TyKind::Func(..) => "function type".to_string(),

            // I just thought this format would look funny 😐
            TyKind::Paren(inner) => format!("{{{}}}", pr_node_kind_str(inner)),
            TyKind::AppExpr(cons, _) | TyKind::App(cons, _) => {
                format!("{} type constructor", pr_node_kind_str(cons))
            },
        }
    }
}
