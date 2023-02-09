use crate::message::message::{Message, MessageBuilder, MessageHolder, MessageStorage};
use crate::parser::token::{Token, TokenKind, TokenStream};
use crate::session::SourceId;
use crate::session::{Session, Stage, StageOutput};

use crate::span::span::{Span, SpanLen, SpanPos, Symbol};

use super::token::{ComplexSymbol, IntKind};

enum TokenStartMatch {
    Ident,
    Num,
    String,
    Skip,
    IndentPrecursor,
    Op,
    Unknown,
}

pub trait LexerCharCheck {
    fn is_ident_first(&self) -> bool;
    fn is_ident_next(&self) -> bool;
    fn is_op(&self) -> bool;
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

    fn is_op(&self) -> bool {
        ['!', '$', '+', '-', '*', '/', '%', '?', '^', '|', '&', '~'].contains(self)
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

impl<'ast> MessageHolder for Lexer {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

pub struct Lexer {
    source_id: SourceId,
    pos: SpanPos,
    token_start_pos: SpanPos,
    tokens: Vec<Token>,
    msg: MessageStorage,
    sess: Session,
    indent_levels: Vec<usize>,
    last_line_begin: SpanPos,
}

impl Lexer {
    pub fn new(source_id: SourceId, sess: Session) -> Self {
        Self {
            source_id,
            pos: 0,
            token_start_pos: 0,
            tokens: Vec::default(),
            msg: MessageStorage::default(),
            sess,
            indent_levels: Default::default(),
            last_line_begin: 0,
        }
    }

    fn source(&self) -> &str {
        self.sess.source_map.get_source(self.source_id).source()
    }

    fn eof(&self) -> bool {
        self.pos as usize >= self.source().len()
    }

    fn peek_by_pos(&self, pos: SpanPos) -> char {
        self.source()
            .chars()
            .nth(pos as usize)
            .expect(format!("Failed to get char from source by index {}", self.pos).as_str())
    }

    fn peek(&self) -> char {
        self.peek_by_pos(self.pos)
    }

    fn lookup(&self) -> Option<char> {
        self.source().chars().nth((self.pos + 1) as usize)
    }

    fn advance_offset(&mut self, offset: SpanLen) -> char {
        let last = self.pos;
        self.pos += offset;
        self.peek_by_pos(last)
    }

    fn advance(&mut self) -> char {
        return self.advance_offset(1);
    }

    fn add_token(&mut self, kind: TokenKind, len: SpanLen) {
        self.tokens.push(Token {
            span: Span::new(self.token_start_pos, len, self.source_id),
            kind,
        });
        self.token_start_pos = self.pos;
    }

    fn add_token_adv(&mut self, kind: TokenKind, len: SpanLen) {
        self.advance_offset(len);
        self.add_token(kind, len);
    }

    fn add_error(&mut self, msg: &str) {
        let span = Span::new(self.token_start_pos, 1, self.source_id);

        MessageBuilder::error()
            .span(span)
            .text(msg.to_string())
            .emit_single_label(self);

        self.tokens.push(Token {
            span,
            kind: TokenKind::Error(Symbol::intern(msg)),
        })
    }

    fn unexpected_token(&mut self) {
        self.add_error("Unexpected token");
        self.advance();
    }

    fn get_fragment_to(&self, start: SpanPos, end: SpanPos) -> (&str, SpanLen) {
        (
            &self.source()[start as usize..end as usize],
            self.pos - start,
        )
    }

    fn get_fragment(&self, start: SpanPos) -> (&str, SpanLen) {
        self.get_fragment_to(start, self.pos)
    }

    fn get_fragment_intern(&mut self, start: SpanPos) -> (Symbol, SpanLen) {
        let (frag, len) = (
            &self.source()[start as usize..self.pos as usize],
            self.pos - start,
        );
        (Symbol::intern(frag), len)
    }

    fn lex_ident(&mut self) {
        let start = self.pos;

        self.advance();

        while !self.eof() && self.peek().is_ident_next() {
            self.advance();
        }

        let (sym, len) = self.get_fragment_intern(start);

        let kind = if let Some(reserved) = TokenKind::try_from_reserved_sym(sym) {
            reserved
        } else {
            TokenKind::Ident(sym)
        };

        self.add_token(kind, len);
    }

    fn lex_op_ident(&mut self) {
        assert!(self.advance() == '(' && self.peek().is_op());

        let start = self.pos;

        while !self.eof() && self.peek() != ')' && self.peek().is_op() {
            self.advance();
        }

        if self.advance() != ')' {
            todo!("error")
        }

        let (sym, len) = self.get_fragment_intern(start);

        self.add_token(TokenKind::OpIdent(sym), len);
    }

    fn lex_op(&mut self) {
        let start = self.pos;

        while !self.eof() && self.advance().is_op() {}

        let (sym, len) = self.get_fragment_intern(start);

        self.add_token(TokenKind::Op(sym), len);
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

        // TODO: Tags
        let (frag, len) = self.get_fragment(start);
        let kind = TokenKind::Int(
            frag.parse().expect("TODO: Check integer lexing"),
            IntKind::Unknown,
        );
        self.add_token(kind, len);
    }

    fn save_source_line(&mut self) {
        self.sess
            .source_map
            .get_source_mut(self.source_id)
            .add_line(self.last_line_begin);
        self.last_line_begin = self.pos;
    }

    fn lex_indent(&mut self) {
        let pos = self.pos;

        while !self.eof() && self.peek().is_indent_precursor() {
            self.save_source_line();

            self.advance();
            self.last_line_begin = self.pos;
        }

        if self.eof() {
            self.add_token(TokenKind::Nl, 1);

            while let Some(_) = self.indent_levels.pop() {
                self.add_token(TokenKind::BlockEnd, 1);
            }
            return;
        }

        // Check that we actually skipped some new-lines
        assert_ne!(pos, self.pos);

        let mut indent_size = 0;

        while !self.eof() && self.peek().is_indent() {
            indent_size += 1;
            self.advance();
        }

        let mut level = *self.indent_levels.last().unwrap_or(&0);

        if indent_size == level {
            self.add_token(TokenKind::Nl, 1);
        } else if indent_size > level {
            self.add_token(TokenKind::BlockStart, indent_size as SpanLen);
            self.indent_levels.push(indent_size);
        } else {
            while indent_size < level {
                self.add_token(TokenKind::BlockEnd, 1);
                self.indent_levels.pop().unwrap();
                level = *self.indent_levels.last().unwrap_or(&0);
                if level < indent_size {
                    self.add_error("Invalid indentation");
                }
            }
        }
    }

    fn lex_multiline_comment(&mut self) {
        self.advance_offset(2);

        while !self.eof() && self.peek() != '*' && self.lookup() != Some('/') {
            self.advance();
        }

        self.advance_offset(2);
    }

    fn lex_line_comment(&mut self) {
        self.advance_offset(2);

        while !self.eof() && self.peek() != '\n' {
            self.advance();
        }
    }
}

impl Stage<TokenStream> for Lexer {
    fn run(mut self) -> StageOutput<TokenStream> {
        while !self.eof() {
            self.token_start_pos = self.pos;
            match self.peek().match_first() {
                TokenStartMatch::Skip => {
                    self.advance();
                },
                TokenStartMatch::Ident => self.lex_ident(),
                TokenStartMatch::Op => self.lex_op(),
                TokenStartMatch::Num => self.lex_num(),
                TokenStartMatch::String => self.lex_str(),
                TokenStartMatch::IndentPrecursor => self.lex_indent(),
                TokenStartMatch::Unknown => {
                    match TokenKind::try_from_chars(self.peek(), self.lookup()) {
                        ComplexSymbol::Punct(kind, len) => {
                            self.add_token_adv(TokenKind::Punct(kind), len)
                        },

                        ComplexSymbol::Kw(kw, len) => self.add_token_adv(TokenKind::Kw(kw), len),

                        ComplexSymbol::LineComment => self.lex_line_comment(),

                        ComplexSymbol::MultilineComment => self.lex_multiline_comment(),

                        ComplexSymbol::None => self.unexpected_token(),

                        ComplexSymbol::OpIdent => self.lex_op_ident(),
                    }
                },
            };
        }

        if self.last_line_begin == 0 {
            self.save_source_line();
        }

        self.save_source_line();
        self.add_token(TokenKind::Eof, 1);

        StageOutput::new(self.sess, TokenStream::new(self.tokens), self.msg)
    }
}
