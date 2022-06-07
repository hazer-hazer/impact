use crate::parser::token::TokenKind;

use super::PP;

// impl<'a> PP<'a> for TokenKind {
//     fn fmt(&self, sess: &'a crate::session::Session) -> &'a str {
//         match self {
//             TokenKind::Eof => "[EOF]",
//             TokenKind::Nl => "\n",
//             TokenKind::Int(val) | TokenKind::String(val) | TokenKind::Ident(val) => sess.get_str(*val),
//             TokenKind::Assign => "=",
//             TokenKind::Add => "+",
//             TokenKind::Sub => "-",
//             TokenKind::Mul => "*",
//             TokenKind::Div => "/",
//             TokenKind::Mod => "%",
//             TokenKind::Colon => ":",
//             TokenKind::Arrow => "->",
//             TokenKind::Unexpected(val) => sess.get_str(*val)
//         }
//     }
// }
