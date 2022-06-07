use crate::span::span::{LSpan, Span, Symbol};

#[derive(PartialEq, Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

#[derive(PartialEq)]
pub enum TokenKind {
    Eof,
    Nl,
    Bool(bool),
    Int(Symbol),
    String(Symbol),
    Ident(Symbol),

    BinOp(BinOp),
}

impl TokenKind {}

pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn located(span: LSpan, kind: TokenKind) -> Self {
        Self {
            span: Span::located(span),
            kind,
        }
    }
}

#[derive(Default)]
pub struct TokenStream(Vec<Token>);

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
