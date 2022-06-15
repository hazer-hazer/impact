use crate::session::Session;

pub mod ast;
pub mod hir;

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
        format!("{}", "    ".repeat(self.indent_level as usize))
    }
}

struct PrettyPrinter<'a> {
    sess: &'a Session,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new(sess: &'a Session) -> Self {
        Self { sess: sess }
    }
}

pub trait PP<'a> {
    fn ppfmt(&self, sess: &'a Session) -> String;
}

impl<'a, T> PP<'a> for Box<T>
where
    T: PP<'a>,
{
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{}", self.as_ref().ppfmt(sess))
    }
}
