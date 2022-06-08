use self::stmt::Stmt;

mod expr;
mod stmt;

type N<T> = Box<T>;

pub struct AST {
    stmts: Vec<Stmt>,
}

impl AST {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }
}
