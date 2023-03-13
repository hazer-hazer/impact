use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    message::message::{MessageHolder, MessageStorage},
    mir::MIR,
    session::{Session, Stage, StageOutput},
};

use super::ctx::CodeGenCtx;

pub struct CodeGen<'ctx> {
    mir: &'ctx MIR,

    sess: Session,
    msg: MessageStorage,
}

impl<'ctx> MessageHolder for CodeGen<'ctx> {
    fn save(&mut self, msg: crate::message::message::Message) {
        self.msg.add_message(msg)
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(sess: Session, mir: &'ctx MIR) -> Self {
        Self {
            sess,
            mir,
            msg: MessageStorage::default(),
        }
    }

    fn codegen_main(&mut self) -> CodeGenCtx<'ctx> {
        let llvm_ctx = &self.sess.llvm_ctx;
        let ctx = CodeGenCtx {
            sess: &self.sess,
            llvm_ctx,
            module: llvm_ctx.create_module("kek"),
            builder: llvm_ctx.create_builder(),
        };

        ctx
    }
}

impl<'ctx> Stage<Module<'ctx>> for CodeGen<'ctx> {
    fn run(mut self) -> StageOutput<Module<'ctx>> {
        let CodeGenCtx { module, .. } = self.codegen_main();
        // FIXME: Rewrite stages to error propagation model
        StageOutput::new(self.sess, module, self.msg)
    }
}
