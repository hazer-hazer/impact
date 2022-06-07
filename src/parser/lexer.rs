use nom::branch::alt;
use nom::bytes::complete::{escaped, tag};
use nom::character::complete::{alphanumeric1, digit1, one_of};
use nom::character::streaming::space0;
use nom::combinator::value;
use nom::combinator::{map, opt};
use nom::error::{ErrorKind, ParseError};
use nom::sequence::{delimited, preceded};
use nom::IResult;

use crate::message::message::MessageHolder;
use crate::parser::token::{Token, TokenKind, TokenStream};

use crate::session::{Session, Stage, StageResult};
use crate::span::span::{Span, SpanLen, Symbol};

use super::token::BinOp;

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
    token_start_pos: u32,
    tokens: Vec<Token>,
    msg: MessageHolder,
    sess: Session,
}

struct LexInput<'a> {
    string: &'a str,
    sess: Session,
}

#[derive(Debug, PartialEq)]
pub enum LexError<I> {
    Nom(I, ErrorKind),
}

impl<I> ParseError<I> for LexError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        LexError::Nom(input, kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

type Result<'a, T = TokenKind> = IResult<&'a str, T, LexError<&'a str>>;

// trait Parser {}
// impl<'a, T: FnMut(&'a str) -> Result> Parser for T {}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, sess: Session) -> Self {
        Self {
            source,
            pos: 0,
            token_start_pos: 0,
            tokens: Vec::default(),
            msg: MessageHolder::default(),
            sess,
        }
    }

    fn bool_lit(input: &'a str) -> Result {
        let t = value(true, tag("true"));
        let f = value(false, tag("false"));
        map(alt((t, f)), |v| TokenKind::Bool(v))(input)
    }

    fn str_lit(&mut self) -> impl FnMut(&'a str) -> Result + '_ {
        |input: &'a str| -> Result {
            let p = escaped(alphanumeric1, '\\', one_of("\"n\\"));

            map(p, |v| TokenKind::String(self.sess.intern(v)))(input)
        }
    }

    fn lit(&mut self) -> impl FnMut(&'a str) -> Result + '_ {
        |input: &'a str| -> Result { alt((Lexer::bool_lit, self.str_lit()))(input) }
    }

    fn bin_op(input: &'a str) -> Result<BinOp> {
        let (i, tok) = one_of("+-*/%")(input)?;

        Ok((
            i,
            match tok {
                '+' => BinOp::Plus,
                '-' => BinOp::Minus,
                '*' => BinOp::Mul,
                '/' => BinOp::Div,
                '%' => BinOp::Mod,
                _ => unreachable!(),
            },
        ))
    }

    fn num(&mut self) -> impl FnMut(&'a str) -> Result + '_ {
        |input: &'a str| {
            map(preceded(tag("-"), digit1), |v| {
                TokenKind::Int(self.sess.intern(v))
            })(input)
        }
    }
}

impl<'a> Stage<TokenStream> for Lexer<'a> {
    fn run(mut self, sess: crate::session::Session) -> StageResult<TokenStream> {
        StageResult::new(self.sess, TokenStream::new(self.tokens), self.msg)
    }
}
