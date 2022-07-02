use crate::{
    ast::expr::{InfixOp, InfixOpKind, PrefixOp, PrefixOpKind},
    parser::token::Punct,
    session::Session,
    span::span::Kw,
};

pub mod ast;
pub mod defs;
pub mod hir;

pub struct AstLikePP<'a> {
    out: String,
    indent_level: u32,
    sess: &'a Session,
}

impl<'a> AstLikePP<'a> {
    pub fn new(sess: &'a Session) -> Self {
        Self {
            out: String::new(),
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

    fn cur_indent(&self) -> &str {
        &"    ".repeat(self.indent_level as usize)
    }

    fn out_indent(&self) {
        self.out.push_str(&self.cur_indent())
    }

    fn word(&mut self, str: &str) {
        self.out.push_str(str);
    }

    fn sword(&mut self, string: String) {
        self.out += &string;
    }

    fn kw(&mut self, kw: Kw) {
        let (pre, post) = match kw {
            Kw::Let | Kw::Type => ("", "\n"),
            Kw::In => (" ", " "),
            Kw::Mod => ("", "\n"),
            Kw::Root | Kw::M | Kw::Unknown => ("", ""),
        };

        self.out.push_str(pre);
        self.out.push_str(&kw.to_string());
        self.out.push_str(post);
    }

    fn punct(&mut self, punct: Punct) {
        let (pre, post) = match punct {
            Punct::Assign | Punct::Arrow => (" ", " "),
            Punct::Colon => ("", " "),
            Punct::Dot | Punct::LParen | Punct::RParen | Punct::Backslash => ("", ""),
        };
    }

    fn sp(&mut self) {
        self.out.push(' ');
    }

    fn nl(&mut self) {
        self.out.push('\n');
    }

    fn infix(&mut self, infix: &InfixOp) {
        let (pre, post) = match infix.node() {
            InfixOpKind::Plus
            | InfixOpKind::Minus
            | InfixOpKind::Mul
            | InfixOpKind::Div
            | InfixOpKind::Mod => (" ", " "),
        };

        self.out.push_str(pre);
        self.out.push_str(&infix.node().to_string());
        self.out.push_str(post);
    }

    fn prefix(&mut self, prefix: &PrefixOp) {
        let post = match prefix.node() {
            PrefixOpKind::Not => "",
        };

        self.out.push_str(&prefix.node().to_string());
        self.out.push_str(post);
    }
}
