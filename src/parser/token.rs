use core::fmt;
use std::fmt::{Debug, Display};

use crate::{
    span::span::{Kw, Span, SpanLen, Symbol, WithSpan},
    typeck::ty::TypeVarId,
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
    Colon,
    Dot,
    LParen,
    RParen,
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
                Punct::Colon => ":",
                Punct::Dot => ".",
                Punct::LParen => "(",
                Punct::RParen => ")",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntKind {
    Unknown,
    Inferred(TypeVarId),

    U8,
    U16,
    U32,
    U64,
    Uint,

    I8,
    I16,
    I32,
    I64,
    Int,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FloatKind {
    Unknown,

    Inferred(TypeVarId),

    F32,
    F64,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TokenKind {
    Eof,
    Nl,
    Bool(bool),
    Int(u64, IntKind),
    Float(f64, FloatKind),
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

pub enum ComplexSymbol {
    LineComment,
    MultilineComment,
    Punct(Punct, SpanLen),
    Infix(Infix, SpanLen),
    None,
}

impl TokenKind {
    pub fn try_from_reserved_sym(sym: Symbol) -> Option<Self> {
        match sym.as_str() {
            "true" => Some(TokenKind::Bool(true)),
            "false" => Some(TokenKind::Bool(false)),
            "not" => Some(TokenKind::Prefix(Prefix::Not)),
            "let" => Some(TokenKind::Kw(Kw::Let)),
            "in" => Some(TokenKind::Kw(Kw::In)),
            "mod" => Some(TokenKind::Kw(Kw::Mod)),
            "type" => Some(TokenKind::Kw(Kw::Type)),
            _ => None,
        }
    }

    pub fn try_from_chars(char1: char, char2: Option<char>) -> ComplexSymbol {
        match (char1, char2) {
            ('/', Some('/')) => ComplexSymbol::LineComment,
            ('/', Some('*')) => ComplexSymbol::MultilineComment,

            ('+', _) => ComplexSymbol::Infix(Infix::Plus, 1),
            ('*', _) => ComplexSymbol::Infix(Infix::Mul, 1),
            ('/', _) => ComplexSymbol::Infix(Infix::Div, 1),
            ('%', _) => ComplexSymbol::Infix(Infix::Mod, 1),
            ('=', _) => ComplexSymbol::Punct(Punct::Assign, 1),
            ('\\', _) => ComplexSymbol::Punct(Punct::Backslash, 1),
            (':', _) => ComplexSymbol::Punct(Punct::Colon, 1),
            ('.', _) => ComplexSymbol::Punct(Punct::Dot, 1),
            ('(', _) => ComplexSymbol::Punct(Punct::LParen, 1),
            (')', _) => ComplexSymbol::Punct(Punct::RParen, 1),

            ('-', Some('>')) => ComplexSymbol::Punct(Punct::Arrow, 2),
            ('-', _) => ComplexSymbol::Infix(Infix::Minus, 1),

            _ => ComplexSymbol::None,
        }
    }
}

impl PartialEq<TokenCmp> for TokenKind {
    fn eq(&self, other: &TokenCmp) -> bool {
        other == self
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenCmp {
    Eof,
    Nl,
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

impl Display for TokenCmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenCmp::Eof => "[EOF]".to_string(),
                TokenCmp::Nl => "[NL]".to_string(),
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
            | (TokenKind::Bool(_), TokenCmp::Bool)
            | (TokenKind::Int(_, _), TokenCmp::Int)
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

impl Display for IntKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntKind::Unknown => "",
                IntKind::Inferred(_) => "",
                IntKind::U8 => "u8",
                IntKind::U16 => "u16",
                IntKind::U32 => "u32",
                IntKind::U64 => "u64",
                IntKind::Uint => "uint",
                IntKind::I8 => "i8",
                IntKind::I16 => "i16",
                IntKind::I32 => "i32",
                IntKind::I64 => "i64",
                IntKind::Int => "int",
            }
        )
    }
}

impl Display for FloatKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FloatKind::Unknown => "",
                FloatKind::Inferred(_) => "",
                FloatKind::F32 => "f32",
                FloatKind::F64 => "f64",
            }
        )
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Eof => write!(f, "{}", "[EOF]"),
            TokenKind::Nl => write!(f, "{}", "[NL]"),
            TokenKind::Int(val, kind) => write!(f, "{}{}", val, kind),
            TokenKind::String(val) | TokenKind::Ident(val) | TokenKind::Error(val) => {
                write!(f, "{}", val)
            },
            TokenKind::Float(val, kind) => write!(f, "{}{}", val, kind),
            TokenKind::Infix(infix) => write!(f, "{}", infix),
            TokenKind::Bool(val) => write!(f, "{}", if *val { "true" } else { "false" }),
            TokenKind::Prefix(prefix) => write!(f, "{}", prefix),
            TokenKind::Kw(kw) => write!(f, "{}", kw),
            TokenKind::Indent => write!(f, "{}", "[INDENT]"),
            TokenKind::Dedent => write!(f, "{}", "[DEDENT]"),
            TokenKind::Punct(punct) => write!(f, "{}", punct),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`{}` --> {}", self.kind, self.span)
    }
}

impl WithSpan for Token {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Default, Debug)]
pub struct TokenStream(pub Vec<Token>);

pub const TOKEN_STREAM_DELIM: &str = " ";

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

impl Display for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<_>>()
                .join(TOKEN_STREAM_DELIM)
        )
    }
}
