use std::fmt::Debug;

use crate::{parser::token::{Infix, Prefix, TokenKind, Token, TokenStream}, session::Session, span::span::Span};

use super::PP;

impl<'a> PP<'a> for TokenKind {
    fn ppfmt(&self, sess: &'a Session) -> String {
        match self {
            TokenKind::Eof => "[EOF]",
            TokenKind::Nl => "\n",
            TokenKind::Int(val) | TokenKind::String(val) | TokenKind::Ident(val) => {
                sess.get_str(*val)
            }
            TokenKind::Infix(infix) => match infix {
                Infix::Plus => "+",
                Infix::Minus => "-",
                Infix::Mul => "*",
                Infix::Div => "/",
                Infix::Mod => "%",
            },
            TokenKind::Error(val) => sess.get_str(*val),
            TokenKind::Bool(val) => {
                if *val {
                    "true"
                } else {
                    "false"
                }
            }
            TokenKind::Prefix(prefix) => match *prefix {
                Prefix::Not => "not",
            },
        }.to_string()
    }
}

impl<'a> PP<'a> for Token {
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{} at {}", self.kind.ppfmt(sess), self.span.ppfmt(sess))
    }
}

impl<'a> PP<'a> for Span {
    fn ppfmt(&self, _: &'a Session) -> String {
        format!("{} [len={}]", self.pos, self.len)
    }
}

impl<'a> PP<'a> for TokenStream {
    fn ppfmt(&self, sess: &'a Session) -> String {
        let mut s = String::new();

        for tok in self.0.iter() {
            s += format!("{}\n", tok.ppfmt(sess)).as_str();
        }

        s
    }
}
