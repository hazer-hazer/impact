use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    message::message::{MessageHolder, MessageStorage},
    mir::MIR,
    session::{Session, Stage, StageOutput},
};

pub struct CodeGen<'ctx> {
    mir: &'ctx MIR,

    llvm_ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    sess: Session,
    msg: MessageStorage,
}

impl<'ctx> MessageHolder for CodeGen<'ctx> {
    fn save(&mut self, msg: crate::message::message::Message) {
        self.msg.add_message(msg)
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(sess: Session, mir: &'ctx MIR, llvm_ctx: &'ctx Context) -> Self {
        Self {
            sess,
            mir,
            llvm_ctx,
            module: llvm_ctx.create_module("kek"),
            builder: llvm_ctx.create_builder(),
            msg: MessageStorage::default(),
        }
    }

    fn codegen_main(&mut self) {}
}

impl<'ctx> Stage<Module<'ctx>> for CodeGen<'ctx> {
    fn run(mut self) -> StageOutput<Module<'ctx>> {
        self.codegen_main();
        // FIXME: Rewrite stages to error propagation model
        StageOutput::new(self.sess, self.module, self.msg)
    }
}
