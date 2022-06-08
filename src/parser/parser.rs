use crate::{
    message::{message::MessageHolder, MessageEmitter},
    session::{Session, Stage, StageResult},
};

use super::{ast::AST, token::TokenStream};

struct Parser {
    sess: Session,
    tokens: TokenStream,
    msg: MessageHolder,
}

impl Parser {
    pub fn new(sess: Session, tokens: TokenStream) -> Self {
        Self {
            sess,
            tokens,
            msg: MessageHolder::default(),
        }
    }
}

impl<'a> Stage<AST> for Parser {
    fn run(self) -> StageResult<AST> {
        StageResult::new(self.sess, AST::new(vec![]), self.msg)
    }

    fn run_and_unwrap(
        self,
        emitter: &mut impl MessageEmitter,
    ) -> crate::session::OkStageResult<AST> {
        self.run().unwrap(emitter)
    }
}
