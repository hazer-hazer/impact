use nom::branch::alt;
use nom::bytes::complete::{escaped, tag};
use nom::character::complete::{alphanumeric1, digit1, one_of};
use nom::character::streaming::space0;
use nom::combinator::{value, map_res};
use nom::combinator::{map, opt};
use nom::error::{ErrorKind, ParseError};
use nom::multi::many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;
use nom_locate::LocatedSpan;

use crate::message::message::MessageHolder;
use crate::parser::token::{Token, TokenKind, TokenStream};

use crate::session::{Session, Stage, StageResult};
use crate::span::span::{LSpan, Span, Symbol};

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

// #[derive(Debug, PartialEq)]
// pub enum LexError<I> {
//     Nom(I, ErrorKind),
// }

// impl<I> ParseError<I> for LexError<I> {
//     fn from_error_kind(input: I, kind: ErrorKind) -> Self {
//         LexError::Nom(input, kind)
//     }

//     fn append(_: I, _: ErrorKind, other: Self) -> Self {
//         other
//     }
// }

type Result<'a, T = Token> = IResult<LSpan<'a>, T>;

// trait Parser {}
// impl<'a, T: FnMut(&'a str) -> Result> Parser for T {}

fn bin_op(input: LSpan) -> Result {
    let (s, v) = one_of("+-*/%")(input)?;

    Ok((
        s,
        Token::located(s, TokenKind::BinOp(match v {
            '+' => BinOp::Plus,
            '-' => BinOp::Minus,
            '*' => BinOp::Mul,
            '/' => BinOp::Div,
            '%' => BinOp::Mod,
            _ => unreachable!(),
        })),
    ))
}

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

    fn bool_lit(input: LSpan) -> Result {
        let t = value(true, tag("true"));
        let f = value(false, tag("false"));

        let (s, v) = alt((t, f))(input)?;

        Ok((s, Token::located(s, TokenKind::Bool(v))))
    }

    fn str_lit(&mut self) -> impl FnMut(LSpan) -> Result + '_ {
        |input: LSpan| -> Result {
            let (s, v) = escaped(alphanumeric1, '\\', one_of("\"n\\"))(input)?;

            Ok((
                s,
                Token::located(s, TokenKind::String(self.sess.intern(v.fragment()))),
            ))
        }
    }

    fn lit(&mut self) -> impl FnMut(LSpan<'a>) -> Result + '_ {
        |input| alt((Lexer::bool_lit, self.str_lit()))(input)
    }

    fn num(&mut self) -> impl FnMut(LSpan) -> Result + '_ {
        |input| {
            let (s, v) = preceded(tag("-"), digit1)(input)?;
            Ok((
                s,
                Token::located(s, TokenKind::Int(self.sess.intern(s.fragment())))
            ))
        }
    }
}

impl<'a> Stage<TokenStream> for Lexer<'a> {
    fn run(mut self, sess: crate::session::Session) -> StageResult<TokenStream> {
        let res = many0(alt((
            self.num(),
            self.lit(),
            bin_op,
        )))(LSpan::new(self.source));

        StageResult::new(self.sess, TokenStream::new(self.tokens), self.msg)
    }
}
