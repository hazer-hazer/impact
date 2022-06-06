use crate::span::span::{
    Span,
    Symbol,
};

#[derive(Debug, PartialEq)]
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

    Unexpected(char),
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s;
        let val = match self {
            TokenKind::Eof => "[EOF]",
            TokenKind::Nl => "\n",
            TokenKind::Int(val) | TokenKind::String(val) | TokenKind::Ident(val) => val.as_str(),
            TokenKind::Assign => "=",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Mod => "%",
            TokenKind::Colon => ":",
            TokenKind::Arrow => "->",
            TokenKind::Unexpected(val) => {
                s = format!("[Unexpected '{}']", val);
                &s
            }
        };
        write!(f, "{}", val)
    }
}

impl TokenKind {
}

#[derive(Debug)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Default, Debug)]
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

impl std::fmt::Display for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|tok| format!("{}", tok)).collect::<Vec<String>>().join("\n"))
    }
}
