use crate::{
    ast::expr::{InfixOp, InfixOpKind, PrefixOp, PrefixOpKind},
    parser::token::Punct,
    session::Session,
    span::span::Kw,
};

pub mod ast;
pub mod defs;
pub mod hir;
pub mod res;

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

    pub fn get_string(self) -> String {
        self.out
    }

    fn ch(&mut self, ch: char) -> &mut Self {
        self.out.push(ch);
        self
    }

    fn sp(&mut self) -> &mut Self {
        self.ch(' ')
    }

    fn nl(&mut self) -> &mut Self {
        self.ch('\n')
    }

    fn str(&mut self, str: &str) -> &mut Self {
        self.out.push_str(str);
        self
    }

    fn string<T>(&mut self, value: T) -> &mut Self
    where
        T: ToString,
    {
        self.str(&value.to_string());
        self
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        assert_ne!(self.indent_level, 0);
        self.indent_level -= 1;
    }

    fn cur_indent(&self) -> String {
        "  ".repeat(self.indent_level as usize)
    }

    fn out_indent(&mut self) -> &mut Self {
        self.str(&self.cur_indent())
    }

    fn line(&mut self, str: &str) -> &mut Self {
        self.out.push_str(str);
        self.nl()
    }

    fn kw(&mut self, kw: Kw) -> &mut Self {
        let (pre, post) = match kw {
            Kw::In => (" ", " "),
            Kw::Mod => ("", " "),
            Kw::Let | Kw::Type | Kw::Root | Kw::M | Kw::Unknown => ("", ""),
        };

        self.str(pre);
        self.str(&kw.to_string());
        self.str(post)
    }

    fn punct(&mut self, punct: Punct) -> &mut Self {
        let (pre, post) = match punct {
            Punct::Assign | Punct::Arrow => (" ", " "),
            Punct::Colon => ("", " "),
            Punct::Dot | Punct::LParen | Punct::RParen | Punct::Backslash => ("", ""),
        };

        self.str(pre);
        self.str(&punct.to_string());
        self.str(post)
    }

    fn infix(&mut self, infix: &InfixOp) -> &mut Self {
        let (pre, post) = match infix.node() {
            InfixOpKind::Plus
            | InfixOpKind::Minus
            | InfixOpKind::Mul
            | InfixOpKind::Div
            | InfixOpKind::Mod => (" ", " "),
        };

        self.str(pre);
        self.str(&infix.node().to_string());
        self.str(post)
    }

    fn prefix(&mut self, prefix: &PrefixOp) -> &mut Self {
        let post = match prefix.node() {
            PrefixOpKind::Not => "",
        };

        self.str(&prefix.node().to_string());
        self.str(post)
    }
}
