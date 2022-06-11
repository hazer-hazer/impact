use crate::message::message::{Message, MessageBuilder, MessageHolder, MessageStorage};
use crate::message::MessageEmitter;
use crate::parser::token::{Token, TokenKind, TokenStream};

use crate::session::{OkStageResult, Session, Stage, StageResult};
use crate::span::span::{Span, SpanLen, SpanPos, Symbol};

use super::token::{Infix, Punct};

pub struct Lexer<'a> {
    source: &'a str,
    pos: SpanPos,
    token_start_pos: SpanPos,
    tokens: Vec<Token>,
    msg: MessageStorage,
    sess: Session,
    last_char: char,
    indent_levels: Vec<usize>,
    last_line_begin: SpanPos,
}

enum TokenStartMatch {
    Ident,
    Num,
    String,
    Skip,
    IndentPrecursor,
    Unknown,
}

trait LexerCharCheck {
    fn is_ident_first(&self) -> bool;
    fn is_ident_next(&self) -> bool;
    fn is_skippable(&self) -> bool;
    fn is_indent(&self) -> bool;
    fn is_indent_precursor(&self) -> bool;
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

    fn is_indent(&self) -> bool {
        *self == ' '
    }

    fn is_indent_precursor(&self) -> bool {
        *self == '\n'
    }

    fn match_first(&self) -> TokenStartMatch {
        // Note: Keep order please, at least for indent and skippable
        if self.is_ident_first() {
            TokenStartMatch::Ident
        } else if self.is_digit(10) {
            TokenStartMatch::Num
        } else if *self == '"' {
            TokenStartMatch::String
        } else if self.is_indent_precursor() {
            TokenStartMatch::IndentPrecursor
        } else if self.is_skippable() {
            TokenStartMatch::Skip
        } else {
            TokenStartMatch::Unknown
        }
    }
}

impl<'a> MessageHolder for Lexer<'a> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, sess: Session) -> Self {
        Self {
            source,
            pos: 0,
            token_start_pos: 0,
            tokens: Vec::default(),
            msg: MessageStorage::default(),
            sess,
            last_char: source.chars().nth(0).unwrap_or('\0'),
            indent_levels: Default::default(),
            last_line_begin: 0,
        }
    }

    fn eof(&self) -> bool {
        self.pos as usize >= self.source.len()
    }

    fn peek_by_pos(&self, pos: SpanPos) -> char {
        self.source
            .chars()
            .nth(pos as usize)
            .expect(format!("Failed to get char from source by index {}", self.pos).as_str())
    }

    fn peek(&self) -> char {
        self.peek_by_pos(self.pos)
    }

    fn lookup(&self) -> Option<char> {
        self.source.chars().nth((self.pos + 1) as usize)
    }

    fn advance_offset(&mut self, offset: SpanLen) -> char {
        let last = self.pos;
        self.last_char = self.peek();
        self.pos += offset;
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
        let span = Span::new(self.token_start_pos, 1);

        MessageBuilder::error()
            .span(span)
            .text(msg.to_string())
            .emit(self);

        self.tokens.push(Token {
            span,
            kind: TokenKind::Error(self.sess.intern(msg)),
        })
    }

    fn unexpected_token(&mut self) {
        self.add_error("Unexpected token");
    }

    fn get_fragment_to(&self, start: SpanPos, end: SpanPos) -> (&str, SpanLen) {
        (&self.source[start as usize..end as usize], self.pos - start)
    }

    fn get_fragment(&self, start: SpanPos) -> (&str, SpanLen) {
        self.get_fragment_to(start, self.pos)
    }

    fn get_fragment_intern(&mut self, start: SpanPos) -> (Symbol, SpanLen) {
        let (frag, len) = (
            &self.source[start as usize..self.pos as usize],
            self.pos - start,
        );
        (self.sess.intern(frag), len)
    }

    fn lex_ident(&mut self) {
        let start = self.pos;
        while !self.eof() && self.peek().is_ident_next() {
            self.advance();
        }

        let (sym, len) = self.get_fragment_intern(start);

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

        let (sym, len) = self.get_fragment_intern(start);
        self.add_token(TokenKind::String(sym), len);
    }

    fn lex_num(&mut self) {
        let start = self.pos;
        while !self.eof() && self.peek().is_digit(10) {
            self.advance();
        }

        let (frag, len) = self.get_fragment(start);
        self.add_token(
            TokenKind::Int(frag.parse().expect("TODO: Check integer lexing")),
            len,
        );
    }

    fn save_source_line(&mut self) {
        // FIXME: Save NL or not to save 🤔
        let line = self
            .get_fragment_to(self.last_line_begin, self.pos - 1)
            .0
            .to_string();
        self.sess
            .source_lines_mut()
            .add_line(line, self.last_line_begin);
        self.last_line_begin = self.pos;
    }

    fn lex_indent(&mut self) {
        let pos = self.pos;
        while self.peek().is_indent_precursor() {
            self.add_token_adv(TokenKind::Nl, 1);
            self.save_source_line();
        }

        if pos == self.pos {
            unreachable!();
        }

        let mut indent_size = 0;

        while !self.eof() && self.peek().is_indent() {
            indent_size += 1;
            self.advance();
        }

        let mut level = *self.indent_levels.last().unwrap_or(&0);

        if indent_size > level {
            self.add_token(TokenKind::Indent, 1);
            self.indent_levels.push(indent_size);
        }

        while !self.eof() && indent_size < level {
            self.add_token(TokenKind::Dedent, 1);
            level = self.indent_levels.pop().unwrap();
            if level < indent_size {
                self.add_error("Invalid indentation");
            }
        }
    }
}

impl<'a> Stage<TokenStream> for Lexer<'a> {
    fn run(mut self) -> StageResult<TokenStream> {
        self.sess
            .source_lines_mut()
            .set_source_size(self.source.len());

        while !self.eof() {
            self.token_start_pos = self.pos as SpanPos;
            match self.peek().match_first() {
                TokenStartMatch::Skip => {
                    self.advance();
                }
                TokenStartMatch::Ident => self.lex_ident(),
                TokenStartMatch::Num => self.lex_num(),
                TokenStartMatch::String => self.lex_str(),
                TokenStartMatch::IndentPrecursor => self.lex_indent(),
                TokenStartMatch::Unknown => match self.peek() {
                    '+' => self.add_token_adv(TokenKind::Infix(Infix::Plus), 1),
                    '*' => self.add_token_adv(TokenKind::Infix(Infix::Mul), 1),
                    '/' => self.add_token_adv(TokenKind::Infix(Infix::Div), 1),
                    '%' => self.add_token_adv(TokenKind::Infix(Infix::Mod), 1),
                    '=' => self.add_token_adv(TokenKind::Punct(Punct::Assign), 1),
                    '\\' => self.add_token_adv(TokenKind::Punct(Punct::Backslash), 1),

                    '-' => match self.lookup() {
                        Some('>') => self.add_token_adv(TokenKind::Punct(Punct::Arrow), 2),
                        _ => self.add_token_adv(TokenKind::Infix(Infix::Minus), 1),
                    },

                    _ => self.unexpected_token(),
                },
            };
        }

        if self.last_line_begin == 0 {
            self.save_source_line();
        }

        self.add_token(TokenKind::Eof, 1);

        StageResult::new(self.sess, TokenStream::new(self.tokens), self.msg)
    }

    fn run_and_unwrap(self) -> OkStageResult<TokenStream> {
        self.run().unwrap()
    }
}
