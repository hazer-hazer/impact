use self::stmt::Stmt;

/**
 * HIR is nothing more than just an unwrapped version of AST, i.e. freed of parse results.
 */
pub mod expr;
pub mod stmt;
pub mod visitor;

pub struct HIR<'a> {
    stmts: Vec<Stmt<'a>>,
}

impl<'a> HIR<'a> {
    pub fn stmts(&self) -> &Vec<Stmt> {
        &self.stmts
    }
}
