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
