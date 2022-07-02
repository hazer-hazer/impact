use std::fmt::Display;

use crate::span::span::Ident;

use self::item::Item;

/**
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
pub mod expr;
pub mod item;
pub mod stmt;
pub mod ty;
pub mod visitor;

pub type N<T> = Box<T>;

pub struct HIR {
    items: Vec<N<Item>>,
}

impl HIR {
    pub fn new(items: Vec<N<Item>>) -> Self {
        Self { items }
    }

    pub fn items(&self) -> &[N<Item>] {
        self.items.as_ref()
    }
}

pub struct Path {
    segments: Vec<Ident>,
}

impl Path {
    pub fn new(segments: Vec<Ident>) -> Self {
        Self { segments }
    }

    pub fn segments(&self) -> &[Ident] {
        self.segments.as_ref()
    }
}

impl Display for Path {
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
