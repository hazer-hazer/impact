use crate::{span::span::{Span, Symbol}, session::Session, pp::PP};

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
