use crate::{span::span::{Span, Symbol}, session::Session};

#[derive(PartialEq, Debug)]
pub enum Infix {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

#[derive(PartialEq, Debug)]
pub enum Prefix {
    Not,
}

#[derive(PartialEq)]
pub enum TokenKind {
    Eof,
    Nl,
    Bool(bool),
    Int(Symbol),
    String(Symbol),
    Ident(Symbol),

    Prefix(Prefix),
    Infix(Infix),

    Error(Symbol),
}

impl TokenKind {
    pub fn try_from_reserved_sym(sess: &Session, sym: Symbol) -> Option<Self> {
        let string = sess.get_str(sym);

        match string {
            "true" => Some(TokenKind::Bool(true)),
            "false" => Some(TokenKind::Bool(false)),
            "not" => Some(TokenKind::Prefix(Prefix::Not)),
            _ => None
        }
    }
}

pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self {
            span,
            kind,
        }
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
