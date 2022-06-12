use std::fmt::{format, Display};

use crate::{
    pp::PP,
    session::Session,
    span::span::{Kw, Span, Symbol, WithSpan},
};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Infix {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Infix::Plus => "+",
                Infix::Minus => "-",
                Infix::Mul => "*",
                Infix::Div => "/",
                Infix::Mod => "%",
            }
        )
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Prefix {
    Not,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Prefix::Not => "not",
            }
        )
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Punct {
    Assign,
    Backslash,
    Arrow,
}

impl Display for Punct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Punct::Assign => "=",
                Punct::Backslash => "\\",
                Punct::Arrow => "->",
            }
        )
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TokenKind {
    Eof,
    Nl,
    Bool(bool),
    Int(i64),
    String(Symbol),
    Kw(Kw),
    Ident(Symbol),

    Prefix(Prefix),
    Infix(Infix),

    Punct(Punct),

    Indent,
    Dedent,

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
            "in" => Some(TokenKind::Kw(Kw::In)),
            _ => None,
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum TokenCmp {
    Eof,
    Nl,
    Nls,
    Bool,
    Int,
    String,
    Ident,
    SomePrefix,
    Prefix(Prefix),
    Infix(Infix),
    Kw(Kw),
    Punct(Punct),
    Indent,
    Dedent,
    Error,
}

impl TokenCmp {
    pub fn is_many(&self) -> bool {
        match self {
            TokenCmp::Nls => true,
            _ => false,
        }
    }
}

impl Display for TokenCmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenCmp::Eof => "[EOF]".to_string(),
                TokenCmp::Nl => "[NL]".to_string(),
                TokenCmp::Nls => "[NL*]".to_string(),
                TokenCmp::Bool => "bool".to_string(),
                TokenCmp::Int => "int".to_string(),
                TokenCmp::String => "string".to_string(),
                TokenCmp::Ident => "ident".to_string(),
                TokenCmp::SomePrefix => "prefix operator".to_string(),
                TokenCmp::Prefix(prefix) => format!("{} prefix operator", prefix),
                TokenCmp::Infix(infix) => format!("{} infix operator", infix),
                TokenCmp::Kw(kw) => format!("{} keyword", kw),
                TokenCmp::Punct(punct) => format!("{} punctuation", punct),
                TokenCmp::Indent => "indent".to_string(),
                TokenCmp::Dedent => "dedent".to_string(),
                TokenCmp::Error => "[ERROR]".to_string(),
            }
        )
    }
}

impl std::cmp::PartialEq<TokenKind> for TokenCmp {
    fn eq(&self, other: &TokenKind) -> bool {
        match (other, self) {
            (TokenKind::Eof, TokenCmp::Eof)
            | (TokenKind::Nl, TokenCmp::Nl)
            | (TokenKind::Nl, TokenCmp::Nls)
            | (TokenKind::Bool(_), TokenCmp::Bool)
            | (TokenKind::Int(_), TokenCmp::Int)
            | (TokenKind::String(_), TokenCmp::String)
            | (TokenKind::Ident(_), TokenCmp::Ident)
            | (TokenKind::Prefix(_), TokenCmp::SomePrefix)
            | (TokenKind::Indent, TokenCmp::Indent)
            | (TokenKind::Dedent, TokenCmp::Dedent)
            | (TokenKind::Error(_), TokenCmp::Error) => true,
            (TokenKind::Punct(punct1), TokenCmp::Punct(punct2)) => punct1 == punct2,
            (TokenKind::Kw(kw1), TokenCmp::Kw(kw2)) => kw1 == kw2,
            (TokenKind::Prefix(prefix1), TokenCmp::Prefix(prefix2)) => prefix1 == prefix2,
            (TokenKind::Infix(infix1), TokenCmp::Infix(infix2)) => infix1 == infix2,
            _ => false,
        }
    }
}

impl<'a> PP<'a> for TokenKind {
    fn ppfmt(&self, sess: &'a Session) -> String {
        match self {
            TokenKind::Eof => "[EOF]".to_string(),
            TokenKind::Nl => "[NL]".to_string(),
            TokenKind::Int(val) => val.to_string(),
            TokenKind::String(val) | TokenKind::Ident(val) => sess.get_str(*val).to_string(),
            TokenKind::Infix(infix) => format!("{}", infix).as_str().to_string(),
            TokenKind::Error(val) => sess.get_str(*val).to_string(),
            TokenKind::Bool(val) => {
                if *val {
                    "true"
                } else {
                    "false"
                }
            }
            .to_string(),
            TokenKind::Prefix(prefix) => format!("{}", prefix),
            TokenKind::Kw(kw) => format!("{}", kw),
            TokenKind::Indent => "[indent]".to_string(),
            TokenKind::Dedent => "[dedent]".to_string(),
            TokenKind::Punct(punct) => match punct {
                Punct::Assign => "=",
                Punct::Backslash => "\\",
                Punct::Arrow => "->",
            }
            .to_string(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
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
        format!("{}", self.kind.ppfmt(sess))
    }
}

impl WithSpan for Token {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Default, Debug)]
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
