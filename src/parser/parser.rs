use crate::{
    message::{
        message::{Message, MessageBuilder, MessageHolder, MessageStorage},
        MessageEmitter,
    },
    pp::PP,
    session::{Session, Stage, StageResult},
    span::span::{Kw, Span},
};

use super::{
    ast::AST,
    token::{Token, TokenKind, TokenStream, TokenCmp},
};

struct Parser {
    sess: Session,
    pos: usize,
    tokens: TokenStream,
    msg: MessageStorage,
}

impl MessageHolder for Parser {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl Parser {
    pub fn new(sess: Session, tokens: TokenStream) -> Self {
        Self {
            sess,
            tokens,
            pos: 0,
            msg: MessageStorage::default(),
        }
    }

    fn eof(&self) -> bool {
        self.peek() == TokenCmp::Eof
    }

    fn peek_tok_at(&self, pos: usize) -> &Token {
        &self.tokens[pos]
    }

    fn peek_tok(&self) -> &Token {
        self.peek_tok_at(self.pos)
    }

    fn peek_at(&self, pos: usize) -> &TokenKind {
        &self.peek_tok_at(pos).kind
    }

    fn peek(&self) -> &TokenKind {
        self.peek_at(self.pos)
    }

    fn span(&self) -> Span {
        self.peek_tok().span
    }

    fn is(&self, cmp: TokenCmp) -> bool {
        cmp == *self.peek()
    }

    fn advance_offset(&mut self, offset: usize) -> &TokenKind {
        let last = self.pos;
        self.pos += offset;
        self.peek_at(last)
    }

    fn advance(&mut self) -> &TokenKind {
        self.advance_offset(1)
    }

    // Sadly, cause of problems with exclusive borrowing,
    // it is simplier to make predicate implicitly check for peek in implementation points
    fn skip_if<F>(&mut self, pred: F, expected: &str)
    where
        F: Fn(&TokenKind) -> bool,
    {
        if pred(self.peek()) {
            self.advance();
        } else {
            MessageBuilder::error()
                .span(self.span())
                .text(format!(
                    "Expected {}, found {}",
                    expected,
                    self.peek().ppfmt(&self.sess)
                ))
                .emit(self);
        }
    }

    fn skip(&mut self, cmp: TokenCmp, expected: &str) {
        self.skip_if(
            |kind| cmp == *kind,
            expected,
        );
    }

    fn skip_kw(&mut self, kw: Kw) {
        self.skip_if(
            |kind| TokenCmp::Kw(kw) == *kind,
            format!("Keyword {}", kw).as_str(),
        )
    }

    fn parse_multiple(&mut self, cmp: TokenCmp) -> Vec<Token> {
        let items: Vec<Token> = Default::default();

        while !self.eof() {
            if self.is(cmp) {
                items.push(*self.peek_tok().clone());
            }
        }

        items
    }

    fn parse_let(&mut self) {
        self.skip_kw(Kw::Let);

        let idents = self.parse_multiple(TokenCmp::Ident);
        
        if (idents.len() > 1) {
            
        }
    }

    fn parse_stmt(&mut self) {
        if self.peek().is_kw(&self.sess, Kw::Let) {
            self.parse_let();
        }
    }

    fn parse(&mut self) -> AST {
        AST::new(vec![])
    }
}

impl<'a> Stage<AST> for Parser {
    fn run(mut self) -> StageResult<AST> {
        let ast = self.parse();
        StageResult::new(self.sess, ast, self.msg)
    }

    fn run_and_unwrap(
        self,
        emitter: &mut impl MessageEmitter,
    ) -> crate::session::OkStageResult<AST> {
        self.run().unwrap(emitter)
    }
}
