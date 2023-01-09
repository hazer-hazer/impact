use std::{collections::HashMap, fmt::Display};

use crate::{
    dt::arena::declare_arena,
    resolve::res::Res,
    span::span::{Ident, Span},
};

use self::item::{Item, ItemId};

/**
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
pub mod expr;
pub mod item;
pub mod pat;
pub mod stmt;
pub mod ty;
pub mod visitor;

declare_arena! {
    expr: super::expr::Expr<'a>,
    item: super::item::Item<'a>,
    pat: super::pat::Pat<'a>,
    stmt: super::stmt::Stmt<'a>,
    ty: super::ty::Ty<'a>,
    block: super::expr::Block<'a>,
    path: super::Path<'a>,
    path_seg: super::PathSeg,
}

pub struct HIR {}

pub struct PathSeg {
    ident: Ident,
    span: Span,
}

impl PathSeg {
    pub fn new(ident: Ident, span: Span) -> Self {
        Self { ident, span }
    }
}

impl Display for PathSeg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

pub struct Path<'hir> {
    res: Res,
    segments: &'hir [&'hir PathSeg],
    span: Span,
}

impl<'hir> Path<'hir> {
    pub fn new(res: Res, segments: &'hir [&'hir PathSeg], span: Span) -> Self {
        Self {
            res,
            segments,
            span,
        }
    }

    pub fn res(&self) -> &Res {
        &self.res
    }

    pub fn segments(&self) -> &[&PathSeg] {
        self.segments.as_ref()
    }
}

impl<'hir> Display for Path<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.segments()
                .iter()
                .map(|seg| format!("{}", seg))
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}
