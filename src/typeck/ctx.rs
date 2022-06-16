use crate::{pp::PP, session::Session, span::span::Ident};

use super::ty::Ty;

#[derive(Clone)]
pub enum CtxItem {
    Var(Ident),
    TypedTerm(Ident, Ty),
    Existential(Ident, Option<Ty>),
    Marker(Ident),
}

pub enum CtxItemName {
    Var(Ident),
    TypedTerm(Ident),
    Existential(Ident),
    Marker(Ident),
}

impl<'a> PP<'a> for CtxItem {
    fn ppfmt(&self, sess: &'a Session) -> String {
        match self {
            CtxItem::Var(ident) => ident.ppfmt(sess),
            CtxItem::TypedTerm(ident, ty) => format!("{}: {}", ident.ppfmt(sess), ty.ppfmt(sess)),
            CtxItem::Existential(ident, solution) => match solution {
                Some(solved) => format!("{}^ = {}", ident.ppfmt(sess), solved.ppfmt(sess)),
                None => format!("{}^", ident.ppfmt(sess)),
            },
            CtxItem::Marker(ident) => format!(">{}", ident.ppfmt(sess)),
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

    pub fn get_item_index(&self, item: &CtxItemName) -> Option<usize> {
        let index = self.items.iter().position(|it| match (item, it) {
            (CtxItemName::Var(ident1), CtxItem::Var(ident2))
            | (CtxItemName::TypedTerm(ident1), CtxItem::TypedTerm(ident2, _))
            | (CtxItemName::Existential(ident1), CtxItem::Existential(ident2, _))
            | (CtxItemName::Marker(ident1), CtxItem::Marker(ident2))
                if ident1 == ident2 =>
            {
                true
            }
            _ => false,
        });

        index
    }

    pub fn contains(&self, item: &CtxItemName) -> bool {
        self.get_item_index(item).is_some()
    }

    pub fn lookup(&self, item: &CtxItemName) -> Option<&CtxItem> {
        match self.get_item_index(item) {
            Some(index) => self.items.get(index),
            None => None,
        }
    }

    /// The context is complete if all existentials inside it are solved
    pub fn is_complete(&self) -> bool {
        for item in &self.items {
            match item {
                CtxItem::Existential(_, solution) if solution.is_none() => return false,
                _ => {}
            }
        }

        true
    }
}
