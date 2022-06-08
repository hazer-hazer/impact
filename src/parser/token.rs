use std::fmt::Display;

use crate::{
    pp::PP,
    session::Session,
    span::span::{Kw, Span, Symbol},
};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Infix {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Prefix {
    Not,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Punct {
    Assign,
}

impl Display for Punct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Punct::Assign => "=",
            }
        )
    }
}

#[derive(PartialEq)]
pub enum TokenKind {
    Eof,
    Nl,
    Bool(bool),
    Int(Symbol),
    String(Symbol),
    Kw(Kw),
    Ident(Symbol),

    Prefix(Prefix),
    Infix(Infix),

    Punct(Punct),

    Error(Symbol),
}

impl TokenKind {
    pub fn try_from_reserved_sym(sess: &Session, sym: Symbol) -> Option<Self> {
        let string = sess.get_str(sym);

        match string {
            "true" => Some(TokenKind::Bool(true)),
            "false" => Some(TokenKind::Bool(false)),
            "not" => Some(TokenKind::Prefix(Prefix::Not)),
            "let" => Some(TokenKind::Kw(Kw::Let)),
            _ => None,
        }
    }

    pub fn as_kw(&self, sess: &Session) -> Option<Kw> {
        match self {
            TokenKind::Ident(sym) => sess.as_kw(*sym),
            _ => None,
        }
    }

    pub fn is_kw(&self, sess: &Session, check: Kw) -> bool {
        if let Some(kw) = self.as_kw(sess) {
            kw == check
        } else {
            false
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum TokenCmp {
    Eof,
    Nl,
    Bool,
    Int,
    String,
    Ident,
    Prefix,
    Infix,
    Kw(Kw),
    Punct(Punct),
    Error,
}

impl std::cmp::PartialEq<TokenKind> for TokenCmp {
    fn eq(&self, other: &TokenKind) -> bool {
        match (other, self) {
            (TokenKind::Eof, TokenCmp::Eof)
            | (TokenKind::Nl, TokenCmp::Nl)
            | (TokenKind::Bool(_), TokenCmp::Bool)
            | (TokenKind::Int(_), TokenCmp::Int)
            | (TokenKind::String(_), TokenCmp::String)
            | (TokenKind::Ident(_), TokenCmp::Ident)
            | (TokenKind::Prefix(_), TokenCmp::Prefix)
            | (TokenKind::Infix(_), TokenCmp::Infix)
            | (TokenKind::Error(_), TokenCmp::Error) => true,
            (TokenKind::Punct(punct1), TokenCmp::Punct(punct2)) => punct1 == punct2,
            (TokenKind::Kw(kw1), TokenCmp::Kw(kw2)) => kw1 == kw2,
            _ => false,
        }
    }
}

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
            TokenKind::Kw(kw) => match kw {
                Kw::Let => "let",
            },
            TokenKind::Punct(punct) => match punct {
                Punct::Assign => "=",
            },
        }
        .to_string()
    }
}

pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }
}

impl<'a> PP<'a> for Token {
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{} at {}", self.kind.ppfmt(sess), self.span.ppfmt(sess))
    }
}

#[derive(Default)]
pub struct TokenStream(pub Vec<Token>);

impl std::ops::Index<usize> for TokenStream {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        self.0
            .get(index)
            .expect(format!("Failed to get token from TokenStream by index {index:}").as_str())
    }
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self(tokens)
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
