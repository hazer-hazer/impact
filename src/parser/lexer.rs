use crate::parser::token::{Token, TokenKind, TokenStream};

use crate::span::span::{Span, SpanLen, Symbol};

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
    token_start_pos: u64,
    tokens: Vec<Token>,
}

trait LexerCharCheck {
    fn is_ident_first(&self) -> bool;
    fn is_ident_next(&self) -> bool;
    fn maybe_unit_token(&self, next: Option<char>) -> Option<(TokenKind, SpanLen)>;
}

impl LexerCharCheck for char {
    fn is_ident_first(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }

    fn is_ident_next(&self) -> bool {
        self.is_ident_first() || self.is_digit(10)
    }

    /// Returns kind of unit token if some and advancement offset
    fn maybe_unit_token(&self, next: Option<char>) -> Option<(TokenKind, SpanLen)> {
        match self {
            '=' => Some((TokenKind::Assign, 1)),
            '+' => Some((TokenKind::Add, 1)),
            '*' => Some((TokenKind::Mul, 1)),
            '/' => Some((TokenKind::Div, 1)),
            '%' => Some((TokenKind::Mod, 1)),
            ':' => Some((TokenKind::Colon, 1)),
            _ => match (self, next) {
                ('-', Some('>')) => Some((TokenKind::Arrow, 2)),
                ('-', _) => Some((TokenKind::Sub, 1)),
                _ => None,
            },
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            pos: 0,
            token_start_pos: 0,
            tokens: Vec::default(),
        }
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.source.chars().count()
    }

    fn peek(&self) -> char {
        self.source
            .chars()
            .nth(self.pos)
            .expect("Lexer went out of source")
    }

    fn advance(&mut self) -> char {
        self.advance_offset(1)
    }

    fn advance_offset(&mut self, offset: u8) -> char {
        let last = self.peek();
        self.pos += offset as usize;
        last
    }

    fn lookup(&self) -> Option<char> {
        self.source.chars().nth(self.pos + 1)
    }

    fn add_token(&mut self, kind: TokenKind, len: SpanLen) {
        self.tokens.push(Token {
            kind,
            span: Span {
                pos: self.token_start_pos,
                len,
            },
        });
    }

    fn lex_ident(&mut self, first: String) {
        let mut ident = first;
        while !self.eof() && self.peek().is_ident_next() {
            ident.push(self.advance());
        }
        self.add_token(
            TokenKind::Ident(Symbol::intern(ident.as_str())),
            ident.len() as u32,
        );
    }

    pub fn lex(mut self) -> TokenStream {
        while !self.eof() {
            let maybe_unit_token = self.peek().maybe_unit_token(self.lookup());
            if let Some(unit_token) = maybe_unit_token {
                self.add_token(unit_token.0, unit_token.1);
                self.advance();
                continue;
            }

            if self.peek().is_ident_first() {
                // IDK WTF it works only when I create string outside of `lex_ident` scope
                // But I wanna kill
                let first = String::from(self.advance());
                self.lex_ident(first);
            } else {
                self.add_token(TokenKind::Unexpected(self.peek()), 1);
                self.advance();
            }
        }

        self.add_token(TokenKind::Eof, 1);

        TokenStream::new(self.tokens)
    }
}
