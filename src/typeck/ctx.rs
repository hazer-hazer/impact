use std::{collections::HashMap, fmt::Display};

use crate::{
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, IndexVec},
    span::span::Ident,
};

use super::ty::Ty;

declare_idx!(ExistentialId, u32, "^{}", Color::Blue);

// #[derive(Clone)]
// pub enum _CtxItem {
//     /// Type variable
//     Var(Ident),
//     /// Under context Ψ, variable χ has type Α
//     TypedTerm(Ident, Ty),
//     /// Possible solved (if type is Some) existential variable
//     Existential(ExistentialId, Option<Ty>),
//     /// Type marker, in CETC it's ‣α^
//     Marker(Ident),
// }

// pub enum CtxItemName {
//     Var(Ident),
//     TypedTerm(Ident),
//     Existential(ExistentialId),
//     Marker(Ident),
// }

// impl Display for CtxItem {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             CtxItem::Var(ident) => write!(f, "{}", ident),
//             CtxItem::TypedTerm(ident, ty) => write!(f, "{}: {}", ident, ty),
//             CtxItem::Existential(ident, solution) => match solution {
//                 Some(solved) => write!(f, "{}^ = {}", ident, solved),
//                 None => write!(f, "{}^", ident),
//             },
//             CtxItem::Marker(ident) => write!(f, ">{}", ident),
//         }
//     }
// }

// #[derive(Clone)]
// pub struct _Ctx {
//     items: Vec<CtxItem>,
// }

// impl _Ctx {
//     pub fn initial() -> Self {
//         Self { items: vec![] }
//     }

//     // Context items //
//     pub fn add(&mut self, item: CtxItem) -> &mut Self {
//         self.items.push(item);
//         self
//     }

//     pub fn add_many(&mut self, items: Vec<CtxItem>) -> &mut Self {
//         self.items.extend(items);
//         self
//     }

//     pub fn append(&mut self, ctx: _Ctx) -> &mut Self {
//         self.add_many(ctx.items)
//     }

//     pub fn pop(&mut self) -> Option<CtxItem> {
//         self.items.pop()
//     }

//     pub fn get_item_index(&self, item: CtxItemName) -> Option<usize> {
//         let index = self.items.iter().position(|it| match (&item, it) {
//             (CtxItemName::Var(ident1), CtxItem::Var(ident2))
//             | (CtxItemName::TypedTerm(ident1), CtxItem::TypedTerm(ident2, _))
//             | (CtxItemName::Marker(ident1), CtxItem::Marker(ident2))
//                 if ident1 == ident2 =>
//             {
//                 true
//             },
//             (CtxItemName::Existential(id1), CtxItem::Existential(id2, _)) if id1 == id2 => true,
//             _ => false,
//         });

//         index
//     }

//     pub fn contains(&self, item: CtxItemName) -> bool {
//         self.get_item_index(item).is_some()
//     }

//     pub fn lookup(&self, item: CtxItemName) -> Option<&CtxItem> {
//         match self.get_item_index(item) {
//             Some(index) => self.items.get(index),
//             None => None,
//         }
//     }

//     pub fn split(&mut self, item: CtxItemName) -> Vec<CtxItem> {
//         if let Some(index) = self.get_item_index(item) {
//             self.items.drain(index..).collect()
//         } else {
//             vec![]
//         }
//     }

//     pub fn replace(&mut self, item: CtxItemName, add_items: Vec<CtxItem>) -> &mut Self {
//         let right = self.split(item);
//         self.add_many(add_items);
//         self.add_many(right);
//         self
//     }

//     pub fn enter(&mut self, marker_name: Ident, items: Vec<CtxItem>) {
//         self.add(CtxItem::Marker(marker_name));
//         self.add_many(items);
//     }

//     pub fn leave(&mut self, marker_name: Ident) {
//         self.split(CtxItemName::Marker(marker_name));
//     }

//     // pub fn leave_unsolved(&mut self, item: CtxItemName) -> Vec<Ident> {
//     //     let right = self.split(item);

//     //     let mut names = vec![];

//     //     for item in right {
//     //         match item {
//     //             CtxItem::Existential(ident, ty) if ty.is_none() => names.push(ident),
//     //             _ => {},
//     //         }
//     //     }

//     //     names
//     // }

//     /// The context is complete if all existentials inside it are solved
//     pub fn is_complete(&self) -> bool {
//         for item in &self.items {
//             match item {
//                 CtxItem::Existential(_, solution) if solution.is_none() => return false,
//                 _ => {},
//             }
//         }

//         true
//     }
// }

#[derive(Default)]
pub struct Ctx {
    existentials: IndexVec<ExistentialId, Option<Ty>>,
    vars: Vec<Ident>,
    terms: HashMap<Ident, Ty>,
}

impl Ctx {
    pub fn new_with_var(var: Ident) -> Self {
        Self {
            existentials: Default::default(),
            vars: vec![var],
            terms: Default::default(),
        }
    }

    // Getters //
    pub fn get_term(&self, name: Ident) -> Option<Ty> {
        self.terms.get(&name).copied()
    }

    pub fn has_var(&self, name: Ident) -> bool {
        self.get_var(name).is_some()
    }

    pub fn get_var(&self, name: Ident) -> Option<Ident> {
        self.vars.iter().find(|var| **var == name).copied()
    }

    pub fn has_ex(&self, id: ExistentialId) -> bool {
        self.get_ex(id).is_some()
    }

    pub fn get_ex(&self, id: ExistentialId) -> Option<ExistentialId> {
        if let Some(ex) = self.existentials.get(id) {
            Some(id)
        } else {
            None
        }
    }

    pub fn get_solution(&self, id: ExistentialId) -> Option<Ty> {
        self.existentials.get_flat(id).copied()
    }

    // Setters //
    pub fn type_term(&mut self, name: Ident, ty: Ty) {
        assert!(self.terms.insert(name, ty).is_none())
    }
}
