use crate::{
    message::{message::MessageHolder, MessageEmitter},
    session::{Session, Stage, StageResult},
};

use super::{ast::AST, token::{TokenStream, TokenKind}};

struct Parser {
    sess: Session,
    pos: usize,
    tokens: TokenStream,
    msg: MessageHolder,
}

impl Parser {
    pub fn new(sess: Session, tokens: TokenStream) -> Self {
        Self {
            sess,
            tokens,
            pos: 0,
            msg: MessageHolder::default(),
        }
    }

    fn peek_at(&self, pos: usize) -> &TokenKind {
        &self.tokens[pos].kind
    }

    fn peek(&self) -> &TokenKind {
        self.peek_at(self.pos)
    }

    fn advance_offset(&mut self, offset: usize) -> &TokenKind {
        let last = self.pos;
        self.pos += offset;
        self.peek_at(last)
    }

    fn advance(&mut self) -> &TokenKind {
        self.advance_offset(1)
    }

    fn parse(&mut self) -> AST {

        AST::new(vec![])
    }
}

impl<'a> Stage<AST> for Parser {
    fn run(mut self) -> StageResult<AST> {
        StageResult::new(self.sess, self.parse(), self.msg)
    }

    fn run_and_unwrap(
        self,
        emitter: &mut impl MessageEmitter,
    ) -> crate::session::OkStageResult<AST> {
        self.run().unwrap(emitter)
    }
}
