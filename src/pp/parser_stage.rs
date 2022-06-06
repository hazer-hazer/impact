use crate::parser::token::TokenKind;

use super::PP;

impl PP for TokenKind {
    fn fmt<'a>(&self, sess: &'a crate::session::Session) -> &'a str {
        match self {
            TokenKind::Eof => "[EOF]",
            TokenKind::Nl => "\n",
            TokenKind::Int(val) | TokenKind::String(val) | TokenKind::Ident(val) => sess.get_str(*val),
            TokenKind::Assign => "=",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Mod => "%",
            TokenKind::Colon => ":",
            TokenKind::Arrow => "->",
            TokenKind::Unexpected(val) => sess.get_str(*val)
        }
    }
}
