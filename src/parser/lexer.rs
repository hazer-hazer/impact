use crate::message::message::MessageHolder;
use crate::parser::token::{Token, TokenKind, TokenStream};

use crate::session::{Session, Stage, StageResult};
use crate::span::span::{Span, Symbol, SpanLen, SpanPos};

use super::token::Infix;

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
    token_start_pos: SpanPos,
    tokens: Vec<Token>,
    msg: MessageHolder,
    sess: Session,
}

struct LexInput<'a> {
    string: &'a str,
    sess: Session,
}


trait LexerCharCheck {
    fn is_ident_first(&self) -> bool;
    fn is_ident_next(&self) -> bool;
}

impl LexerCharCheck for char {
    fn is_ident_first(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }

    fn is_ident_next(&self) -> bool {
        self.is_ident_first() || self.is_digit(10)
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

    fn peek_by_pos(&self, pos: usize) -> char {
        self.source.chars().nth(pos).expect(format!("Failed to get char from source by index {}", self.pos).as_str())
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

    fn advance_offset(&mut self, offset: u8) -> char {
        let last = self.pos;
        self.pos += offset as usize;
        self.peek_by_pos(last)
    }

    fn advance(&mut self) -> char {
        return self.advance_offset(1)
    }

    fn add_token(&mut self, kind: TokenKind, len: SpanLen) {
        self.tokens.push(Token { span: Span::new(self.token_start_pos, len), kind });
    }

    fn lex_ident(&mut self) {
        let start = self.pos;
        while !self.eof() && self.peek().is_ident_next() {
            self.advance();
        }

        let string = self.slice(start);
        let sym = self.sess.intern(string);
        self.add_token(
            TokenKind::Ident(sym),
            self.pos as SpanLen - start as SpanLen,
        );
    }
}

impl<'a> Stage<TokenStream> for Lexer<'a> {
    fn run(mut self) -> StageResult<TokenStream> {
        while !self.eof() {
            if self.peek().is_ident_first() {
                
            }

            match self.peek() {
                '+' => self.add_token(TokenKind::Infix(Infix::Plus), 1),
                '-' => self.add_token(TokenKind::Infix(Infix::Minus), 1),
                '*' => self.add_token(TokenKind::Infix(Infix::Mul), 1),
                '/' => self.add_token(TokenKind::Infix(Infix::Div), 1),
                '%' => self.add_token(TokenKind::Infix(Infix::Mod), 1),

                _ => unreachable!(),
            };
        }

        StageResult::new(self.sess, TokenStream::new(self.tokens), self.msg)
    }
}
