use crate::session::Session;

pub mod ast;
pub mod hir;
pub mod defs;

pub struct AstLikePP<'a> {
    indent_level: u32,
    sess: &'a Session,
}

impl<'a> AstLikePP<'a> {
    pub fn new(sess: &'a Session) -> Self {
        Self {
            indent_level: 0,
            sess,
        }
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        assert_ne!(self.indent_level, 0);
        self.indent_level -= 1;
    }

    fn cur_indent(&self) -> String {
        "    ".repeat(self.indent_level as usize)
    }
}
