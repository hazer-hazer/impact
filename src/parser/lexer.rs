use crate::message::MessageEmitter;
use crate::message::message::MessageHolder;
use crate::parser::token::{Token, TokenKind, TokenStream};

use crate::session::{Session, Stage, StageResult, OkStageResult};
use crate::span::span::{Span, SpanLen, SpanPos, Symbol};

use super::token::{Infix, Prefix};

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
    token_start_pos: SpanPos,
    tokens: Vec<Token>,
    msg: MessageHolder,
    sess: Session,
}

enum TokenStartMatch {
    Ident,
    Num,
    String,
    Skip,
    Unknown,
}

trait LexerCharCheck {
    fn is_ident_first(&self) -> bool;
    fn is_ident_next(&self) -> bool;
    fn is_skippable(&self) -> bool;
    fn match_first(&self) -> TokenStartMatch;
}

impl LexerCharCheck for char {
    fn is_ident_first(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }

    fn is_ident_next(&self) -> bool {
        self.is_ident_first() || self.is_digit(10)
    }

    fn is_skippable(&self) -> bool {
        self.is_whitespace()
    }

    fn match_first(&self) -> TokenStartMatch {
        if self.is_ident_first() {
            TokenStartMatch::Ident
        } else if self.is_digit(10) {
            TokenStartMatch::Num
        } else if *self == '"' {
            TokenStartMatch::String
        } else if self.is_skippable() {
            TokenStartMatch::Skip
        } else {
            TokenStartMatch::Unknown
        }
    }
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

    fn eof(&self) -> bool {
        self.pos >= self.source.len()
    }

    // fn take_while<F>(&self, mut predicate: F) -> (&str, usize)
    // where F: FnMut(char) -> bool
    // {
    //     let mut offset = 0;

    //     for c in self.source.chars() {
    //         if !predicate(c) {
    //             break;
    //         }

    //         offset += c.len_utf8();
    //     }

    //     if offset == self.pos {
    //         panic
    //     }
    // }

    fn peek_by_pos(&self, pos: usize) -> char {
        self.source
            .chars()
            .nth(pos)
            .expect(format!("Failed to get char from source by index {}", self.pos).as_str())
    }

    fn peek(&self) -> char {
        self.peek_by_pos(self.pos)
    }

    fn slice_end(&self, start: usize, end: usize) -> &str {
        &self.source[start..end]
    }

    fn slice(&self, start: usize) -> &str {
        &self.source[start..self.pos]
    }

    fn advance_offset(&mut self, offset: SpanLen) -> char {
        let last = self.pos;
        self.pos += offset as usize;
        self.peek_by_pos(last)
    }

    fn advance(&mut self) -> char {
        return self.advance_offset(1);
    }

    fn add_token(&mut self, kind: TokenKind, len: SpanLen) {
        self.tokens.push(Token {
            span: Span::new(self.token_start_pos, len),
            kind,
        });
    }

    fn add_token_adv(&mut self, kind: TokenKind, len: SpanLen) {
        self.add_token(kind, len);
        self.advance_offset(len);
    }

    fn add_error(&mut self, msg: &str) {
        self.tokens.push(Token {
            span: Span::new(self.token_start_pos, 1),
            kind: TokenKind::Error(self.sess.intern(msg)),
        })
    }

    fn get_fragment(&mut self, start: usize) -> (Symbol, SpanLen) {
        (self.sess.intern(&self.source[start..self.pos]), self.pos as SpanPos - start as SpanPos)
    }

    fn lex_ident(&mut self) {
        let start = self.pos;
        while !self.eof() && self.peek().is_ident_next() {
            self.advance();
        }

        let (sym, len) = self.get_fragment(start);

        let kind = if let Some(reserved) = TokenKind::try_from_reserved_sym(&self.sess, sym) {
            reserved
        } else {
            TokenKind::Ident(sym)
        };

        self.add_token(kind, len);
    }

    fn lex_str(&mut self) {
        let quote = self.advance();
        let start = self.pos;

        while !self.eof() && self.peek() != quote {
            self.advance();
        }

        if self.peek() != quote {
            self.add_error(format!("Expected closing quote {}", quote).as_str());
        } else {
            self.advance();
        }

        let (sym, len) = self.get_fragment(start);

        self.add_token(TokenKind::String(sym), len);
    }

    fn lex_num(&mut self) {
        let start = self.pos;
        while !self.eof() && self.peek().is_digit(10) {
            self.advance();
        }

        let sym = self.sess.intern(&self.source[start..self.pos]);
        self.add_token(TokenKind::Int(sym), self.pos as SpanLen - start as SpanLen)
    }
}

impl<'a> Stage<TokenStream> for Lexer<'a> {
    fn run(mut self) -> StageResult<TokenStream> {
        while !self.eof() {
            self.token_start_pos = self.pos as SpanPos;
            match self.peek().match_first() {
                TokenStartMatch::Skip => {self.advance();},
                TokenStartMatch::Ident => self.lex_ident(),
                TokenStartMatch::Num => self.lex_num(),
                TokenStartMatch::String => self.lex_str(),
                TokenStartMatch::Unknown => match self.peek() {
                    '+' => self.add_token_adv(TokenKind::Infix(Infix::Plus), 1),
                    '-' => self.add_token_adv(TokenKind::Infix(Infix::Minus), 1),
                    '*' => self.add_token_adv(TokenKind::Infix(Infix::Mul), 1),
                    '/' => self.add_token_adv(TokenKind::Infix(Infix::Div), 1),
                    '%' => self.add_token_adv(TokenKind::Infix(Infix::Mod), 1),
                    '\n' => self.add_token_adv(TokenKind::Nl, 1),

                    _ => unreachable!("'{}'", self.peek()),
                },
            };
        }

        self.add_token(TokenKind::Eof, 1);

        StageResult::new(self.sess, TokenStream::new(self.tokens), self.msg)
    }

    fn run_and_unwrap(self, emitter: &mut impl MessageEmitter) -> OkStageResult<TokenStream> {
        self.run().unwrap(emitter)
    }
}
