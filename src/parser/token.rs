use core::fmt;
use std::fmt::{Debug, Display};

use super::lexer::LexerCharCheck;
use crate::{
    dt::maps::enum_str_map,
    span::{
        impl_with_span,
        sym::{Ident, Internable, Kw, Symbol},
        Span, SpanLen, WithSpan,
    },
};

enum_str_map! {
    #[derive(PartialEq, Debug, Clone, Copy)]
    pub Op {
        Assign: "=",
        Plus: "+",
        Minus: "-",
        Mul: "*",
        Div: "/",
        Mod: "%",
        BitOr: "|",
    }
}

enum_str_map! {
    #[derive(PartialEq, Debug, Clone, Copy)]
    pub Punct {
        Backslash: "\\",
        Arrow: "->",
        Colon: ":",
        Dot: ".",
        LParen: "(",
        RParen: ")",
        Comma: ",",
        Semi: ";",
    }
}

// TODO: Generalize `IntKind` to any suffix
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

// TODO: Generalize `FloatKind` to any suffix
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

enum_str_map! {
    #[derive(Clone, Copy, PartialEq, Debug)]
    pub Bool {
        True: "true",
        False: "false",
    }
}

impl Into<bool> for Bool {
    fn into(self) -> bool {
        match self {
            Bool::True => true,
            Bool::False => false,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenKind {
    Eof,
    Nl,
    BlockStart,
    BlockEnd,

    Bool(Bool),
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

impl Into<TokenKind> for Bool {
    fn into(self) -> TokenKind {
        TokenKind::Bool(self)
    }
}

impl Into<TokenKind> for Kw {
    fn into(self) -> TokenKind {
        TokenKind::Kw(self)
    }
}

#[derive(Debug)]
pub enum IdentIntoTokenErr {
    IntWithKind,
    Unreachable,
}

impl TryFrom<Token> for Ident {
    type Error = IdentIntoTokenErr;

    fn try_from(tok: Token) -> Result<Ident, Self::Error> {
        Ok(Ident::new(
            tok.span,
            match tok.kind {
                TokenKind::Kw(Kw::Unit) => "()".intern(),
                TokenKind::OpIdent(sym) | TokenKind::Ident(sym) => sym,
                TokenKind::Int(val, kind) if kind == IntKind::Unknown => val.to_string().intern(),
                TokenKind::Int(..) => return Err(IdentIntoTokenErr::IntWithKind),
                _ => return Err(IdentIntoTokenErr::Unreachable),
            },
        ))
    }
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
            &TokenKind::Bool(val) => write!(f, "{}", if val.into() { "true" } else { "false" }),
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
    // If after `(` we have whitespace-like char,
    //  it is possibly a unit (`()`) if nothing is between parentheses
    MaybeUnit,
    None,
}

impl TokenKind {
    pub fn try_from_reserved_sym(sym: Symbol) -> Option<Self> {
        Kw::try_from(sym.as_str())
            .map(Into::into)
            .or_else(|str| Bool::try_from(str).map(Into::into))
            .ok()
    }

    pub fn try_from_chars(char1: char, char2: Option<char>) -> ComplexSymbol {
        match (char1, char2) {
            ('(', Some(')')) => ComplexSymbol::Kw(Kw::Unit, 2),
            ('(', Some(next)) if next.is_whitespace() => ComplexSymbol::MaybeUnit,

            ('(', Some(next)) if next.is_custom_op() => ComplexSymbol::OpIdent,

            ('/', Some('/')) => ComplexSymbol::LineComment,
            ('/', Some('*')) => ComplexSymbol::MultilineComment,

            ('\\', _) => ComplexSymbol::Punct(Punct::Backslash, 1),
            (':', _) => ComplexSymbol::Punct(Punct::Colon, 1),
            ('.', _) => ComplexSymbol::Punct(Punct::Dot, 1),
            ('(', _) => ComplexSymbol::Punct(Punct::LParen, 1),
            (')', _) => ComplexSymbol::Punct(Punct::RParen, 1),
            (',', _) => ComplexSymbol::Punct(Punct::Comma, 1),
            (';', _) => ComplexSymbol::Punct(Punct::Semi, 1),

            ('-', Some('>')) => ComplexSymbol::Punct(Punct::Arrow, 2),

            ('=', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Assign, 1),
            ('+', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Plus, 1),
            ('-', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Minus, 1),
            ('*', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Mul, 1),
            ('/', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Div, 1),
            ('%', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::Mod, 1),
            ('|', Some(next)) if !next.is_custom_op() => ComplexSymbol::Op(Op::BitOr, 1),

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
    // PathFirst,
    Op(Op),
    InfixOp,
    Kw(Kw),
    Punct(Punct),
    Semi,
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
                // TokenCmp::PathFirst => "path first segment".to_string(),
                TokenCmp::Op(op) => format!("operator `{}`", op),
                TokenCmp::InfixOp => "some infix operator".to_string(),
                TokenCmp::Kw(kw) => format!("{} keyword", kw),
                TokenCmp::Punct(punct) => format!("{} punctuation", punct),
                TokenCmp::BlockStart => "[BLOCK START]".to_string(),
                TokenCmp::BlockEnd => "[BLOCK END]".to_string(),
                TokenCmp::Semi => "semi".to_string(),
                TokenCmp::Error => "[ERROR]".to_string(),
            }
        )
    }
}

impl std::cmp::PartialEq<TokenKind> for [TokenCmp] {
    fn eq(&self, other: &TokenKind) -> bool {
        self.iter().all(|cmp| cmp == other)
    }
}

impl std::cmp::PartialEq<TokenKind> for TokenCmp {
    fn eq(&self, other: &TokenKind) -> bool {
        match (other, self) {
            (TokenKind::Eof, TokenCmp::Eof)
            | (TokenKind::Nl, TokenCmp::Nl)
            | (TokenKind::Bool(_), TokenCmp::Bool)
            | (TokenKind::Int(..), TokenCmp::Int)
            | (TokenKind::Float(..), TokenCmp::Float)
            | (TokenKind::String(_), TokenCmp::String)
            | (TokenKind::Ident(_), TokenCmp::Ident)
            | (TokenKind::OpIdent(_), TokenCmp::OpIdent)
            | (
                TokenKind::OpIdent(_) | TokenKind::Ident(_) | TokenKind::Kw(Kw::Unit),
                TokenCmp::DeclName,
            )
            | (TokenKind::BlockStart, TokenCmp::BlockStart)
            | (TokenKind::BlockEnd, TokenCmp::BlockEnd)
            | (
                TokenKind::Op(Op::Assign | Op::Plus | Op::Minus | Op::Mul | Op::Div | Op::Mod)
                | TokenKind::CustomOp(_),
                TokenCmp::InfixOp,
            )
            | (TokenKind::Punct(Punct::Semi) | TokenKind::Nl, TokenCmp::Semi)
            | (TokenKind::Error(_), TokenCmp::Error) => true,
            // (TokenKind::Ident(name), TokenCmp::PathFirst) if name.is_ty() => true,
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

impl_with_span!(Token);

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
