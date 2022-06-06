use crate::span::span::{
    Span,
    Symbol,
};

#[derive(PartialEq)]
pub enum TokenKind {
    Eof,
    Nl,
    Int(Symbol),
    String(Symbol),
    Ident(Symbol),

    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Colon,
    Arrow,

    Unexpected(Symbol),
}

impl TokenKind {
}

pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Default)]
pub struct TokenStream(Vec<Token>);

impl std::ops::Index<usize> for TokenStream {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        self.0.get(index).expect(format!("Failed to get token from TokenStream by index {index:}").as_str())
    }
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self(tokens)
    }
}
