use std::fmt::Display;

use crate::{
    ast::{
        self,
        expr::{Block, Expr, ExprKind, Lit, TyExpr},
        MappedAst, Path, WithNodeId,
    },
    cli::color::Colorize,
    parser::token,
    resolve::{
        def::DefKind,
        res::{NamePath, ResKind},
    },
    session::Session,
    span::span::{Ident, Kw, Symbol},
};

use super::ty::{FloatKind, IntKind, PrimTy, Ty, TyError, TyKind, TyResult};

pub type ExistentialIdInner = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExistentialId(ExistentialIdInner);

impl ExistentialId {
    pub fn new(id: ExistentialIdInner) -> Self {
        Self(id)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl Display for ExistentialId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("^{}", self.0).blue())
    }
}

#[derive(Clone)]
pub enum CtxItem {
    /// Type variable
    Var(Ident),
    /// Under context Ψ, variable χ has type Α
    TypedTerm(Ident, Ty),
    /// Possible solved (if type is Some) existential variable
    Existential(ExistentialId, Option<Ty>),
    /// Type marker, in CETC it's ‣α^
    Marker(Ident),
}

pub enum CtxItemName {
    Var(Ident),
    TypedTerm(Ident),
    Existential(ExistentialId),
    Marker(Ident),
}

impl Display for CtxItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CtxItem::Var(ident) => write!(f, "{}", ident),
            CtxItem::TypedTerm(ident, ty) => write!(f, "{}: {}", ident, ty),
            CtxItem::Existential(ident, solution) => match solution {
                Some(solved) => write!(f, "{}^ = {}", ident, solved),
                None => write!(f, "{}^", ident),
            },
            CtxItem::Marker(ident) => write!(f, ">{}", ident),
        }
    }
}

#[derive(Clone)]
pub struct Ctx {
    items: Vec<CtxItem>,
}

impl Ctx {
    pub fn initial() -> Self {
        Self { items: vec![] }
    }

    // Context items //
    pub fn add(&mut self, item: CtxItem) -> &mut Self {
        self.items.push(item);
        self
    }

    pub fn add_many(&mut self, items: Vec<CtxItem>) -> &mut Self {
        self.items.extend(items);
        self
    }

    pub fn append(&mut self, ctx: Ctx) -> &mut Self {
        self.add_many(ctx.items)
    }

    pub fn pop(&mut self) -> Option<CtxItem> {
        self.items.pop()
    }

    pub fn get_item_index(&self, item: CtxItemName) -> Option<usize> {
        let index = self.items.iter().position(|it| match (&item, it) {
            (CtxItemName::Var(ident1), CtxItem::Var(ident2))
            | (CtxItemName::TypedTerm(ident1), CtxItem::TypedTerm(ident2, _))
            | (CtxItemName::Marker(ident1), CtxItem::Marker(ident2))
                if ident1 == ident2 =>
            {
                true
            },
            (CtxItemName::Existential(id1), CtxItem::Existential(id2, _)) if id1 == id2 => true,
            _ => false,
        });

        index
    }

    pub fn contains(&self, item: CtxItemName) -> bool {
        self.get_item_index(item).is_some()
    }

    pub fn lookup(&self, item: CtxItemName) -> Option<&CtxItem> {
        match self.get_item_index(item) {
            Some(index) => self.items.get(index),
            None => None,
        }
    }

    pub fn split(&mut self, item: CtxItemName) -> Vec<CtxItem> {
        if let Some(index) = self.get_item_index(item) {
            self.items.drain(index..).collect()
        } else {
            vec![]
        }
    }

    pub fn replace(&mut self, item: CtxItemName, add_items: Vec<CtxItem>) -> &mut Self {
        let right = self.split(item);
        self.add_many(add_items);
        self.add_many(right);
        self
    }

    pub fn enter(&mut self, marker_name: Ident, items: Vec<CtxItem>) {
        self.add(CtxItem::Marker(marker_name));
        self.add_many(items);
    }

    pub fn leave(&mut self, marker_name: Ident) {
        self.split(CtxItemName::Marker(marker_name));
    }

    // pub fn leave_unsolved(&mut self, item: CtxItemName) -> Vec<Ident> {
    //     let right = self.split(item);

    //     let mut names = vec![];

    //     for item in right {
    //         match item {
    //             CtxItem::Existential(ident, ty) if ty.is_none() => names.push(ident),
    //             _ => {},
    //         }
    //     }

    //     names
    // }

    /// The context is complete if all existentials inside it are solved
    pub fn is_complete(&self) -> bool {
        for item in &self.items {
            match item {
                CtxItem::Existential(_, solution) if solution.is_none() => return false,
                _ => {},
            }
        }

        true
    }
}
