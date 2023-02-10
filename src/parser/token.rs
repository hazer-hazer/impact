use core::fmt;
use std::fmt::{Debug, Display};

use crate::span::span::{Kw, Span, SpanLen, Symbol, WithSpan};

use super::lexer::LexerCharCheck;

// #[derive(PartialEq, Debug, Clone, Copy)]
// pub enum Infix {
//     Plus,
//     Minus,
//     Mul,
//     Div,
//     Mod,
// }

// impl Display for Infix {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             match self {
//                 Infix::Plus => "+",
//                 Infix::Minus => "-",
//                 Infix::Mul => "*",
//                 Infix::Div => "/",
//                 Infix::Mod => "%",
//             }
//         )
//     }
// }

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Assign,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

impl Op {
    pub fn try_from_sym(sym: Symbol) -> Option<Self> {
        match sym.as_str() {
            "=" => Some(Self::Assign),
            "+" => Some(Self::Plus),
            "-" => Some(Self::Minus),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "%" => Some(Self::Mod),
            _ => None,
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Assign => "=",
                Op::Plus => "+",
                Op::Minus => "-",
                Op::Mul => "*",
                Op::Div => "/",
                Op::Mod => "%",
            }
        )
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Punct {
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

impl Display for IntKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntKind::Unknown => "",
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FloatKind {
    Unknown,

    F32,
    F64,
}

impl Display for FloatKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FloatKind::Unknown => "",
                FloatKind::F32 => "f32",
                FloatKind::F64 => "f64",
            }
        )
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenKind {
    Eof,
    Nl,
    BlockStart,
    BlockEnd,

    Bool(bool),
    Int(u64, IntKind),
    Float(f64, FloatKind),
    String(Symbol),
    Kw(Kw),

    Ident(Symbol),

    // (op)
    OpIdent(Symbol),

    // Predefined operators
    Op(Op),

    // Custom operators
    CustomOp(Symbol),

    Punct(Punct),

    Error(Symbol),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Eof => write!(f, "{}", "[EOF]"),
            TokenKind::Nl => write!(f, "{}", "[NL]"),
            TokenKind::Int(val, kind) => write!(f, "{}{}", val, kind),
            TokenKind::String(val) => write!(f, "\"{}\"", val),
            TokenKind::OpIdent(val) => write!(f, "({})", val),
            TokenKind::Ident(val) | TokenKind::Error(val) => {
                write!(f, "{}", val)
            },
            TokenKind::Float(val, kind) => write!(f, "{}{}", val, kind),
            TokenKind::Bool(val) => write!(f, "{}", if *val { "true" } else { "false" }),
            TokenKind::Kw(kw) => write!(f, "{}", kw),
            TokenKind::BlockStart => write!(f, "{}", "[BLOCK START]"),
            TokenKind::BlockEnd => write!(f, "{}", "[BLOCK END]"),
            TokenKind::Punct(punct) => write!(f, "{}", punct),
            TokenKind::Op(op) => write!(f, "{}", op),
            TokenKind::CustomOp(op) => write!(f, "{}", op),
        }
    }
}

pub enum ComplexSymbol {
    LineComment,
    MultilineComment,
    Punct(Punct, SpanLen),
    OpIdent,
    Op(Op, SpanLen),
    Kw(Kw, SpanLen),
    None,
}

impl TokenKind {
    pub fn try_from_reserved_sym(sym: Symbol) -> Option<Self> {
        match sym.as_str() {
            "true" => Some(TokenKind::Bool(true)),
            "false" => Some(TokenKind::Bool(false)),
            "let" => Some(TokenKind::Kw(Kw::Let)),
            "in" => Some(TokenKind::Kw(Kw::In)),
            "mod" => Some(TokenKind::Kw(Kw::Mod)),
            "type" => Some(TokenKind::Kw(Kw::Type)),
            _ => None,
        }
    }

    pub fn try_from_chars(char1: char, char2: Option<char>) -> ComplexSymbol {
        match (char1, char2) {
            ('(', Some(next)) if next.is_custom_op() => ComplexSymbol::OpIdent,

            ('/', Some('/')) => ComplexSymbol::LineComment,
            ('/', Some('*')) => ComplexSymbol::MultilineComment,

            ('\\', _) => ComplexSymbol::Punct(Punct::Backslash, 1),
            (':', _) => ComplexSymbol::Punct(Punct::Colon, 1),
            ('.', _) => ComplexSymbol::Punct(Punct::Dot, 1),
            ('(', _) => ComplexSymbol::Punct(Punct::LParen, 1),
            (')', _) => ComplexSymbol::Punct(Punct::RParen, 1),

            ('-', Some('>')) => ComplexSymbol::Punct(Punct::Arrow, 2),

            ('=', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Assign, 1),
            ('+', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Plus, 1),
            ('-', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Minus, 1),
            ('*', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Mul, 1),
            ('/', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Div, 1),
            ('%', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Mod, 1),

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
    BlockStart,
    BlockEnd,
    Bool,
    Int,
    Float,
    String,
    Ident,
    OpIdent,
    DeclName,
    Op(Op),
    InfixOp,
    Kw(Kw),
    Punct(Punct),
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
                TokenCmp::Float => "float".to_string(),
                TokenCmp::String => "string".to_string(),
                TokenCmp::Ident => "ident".to_string(),
                TokenCmp::OpIdent => "operator ident".to_string(),
                TokenCmp::DeclName => "declaration name".to_string(),
                TokenCmp::Op(op) => format!("operator `{}`", op),
                TokenCmp::InfixOp => "some infix operator".to_string(),
                TokenCmp::Kw(kw) => format!("{} keyword", kw),
                TokenCmp::Punct(punct) => format!("{} punctuation", punct),
                TokenCmp::BlockStart => "[BLOCK START]".to_string(),
                TokenCmp::BlockEnd => "[BLOCK END]".to_string(),
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
            | (TokenKind::Float(_, _), TokenCmp::Float)
            | (TokenKind::String(_), TokenCmp::String)
            | (TokenKind::Ident(_), TokenCmp::Ident)
            | (TokenKind::OpIdent(_), TokenCmp::OpIdent)
            | (TokenKind::OpIdent(_) | TokenKind::Ident(_), TokenCmp::DeclName)
            | (TokenKind::BlockStart, TokenCmp::BlockStart)
            | (TokenKind::BlockEnd, TokenCmp::BlockEnd)
            | (
                TokenKind::Op(Op::Assign | Op::Plus | Op::Minus | Op::Mul | Op::Div | Op::Mod)
                | TokenKind::CustomOp(_),
                TokenCmp::InfixOp,
            )
            | (TokenKind::Error(_), TokenCmp::Error) => true,
            (TokenKind::Punct(punct1), TokenCmp::Punct(punct2)) => punct1 == punct2,
            (TokenKind::Kw(kw1), TokenCmp::Kw(kw2)) => kw1 == kw2,
            (TokenKind::Op(op1), TokenCmp::Op(op2)) => op1 == op2,
            _ => false,
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
