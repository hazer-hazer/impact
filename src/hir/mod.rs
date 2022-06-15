use self::stmt::Stmt;

/**
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
pub mod expr;
pub mod stmt;
pub mod ty;
pub mod visitor;

pub type N<T> = Box<T>;

pub struct HIR {
    stmts: Vec<N<Stmt>>,
}

impl HIR {
    pub fn new(stmts: Vec<N<Stmt>>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &Vec<N<Stmt>> {
        &self.stmts
    }
}
