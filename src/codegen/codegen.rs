use inkwell::context::Context;

use crate::{
    hir::HIR,
    message::message::{MessageHolder, MessageStorage},
    mir::MIR,
    session::{Session, Stage, StageOutput},
};

use super::{body::BodyCodeGen, ctx::CodeGenCtx, func::FunctionsCodeGen};

pub struct CodeGen<'ink, 'ctx> {
    mir: &'ctx MIR,
    hir: &'ctx HIR,

    llvm_ctx: &'ink Context,

    sess: Session,
    msg: MessageStorage,
}

impl<'ink, 'ctx> MessageHolder for CodeGen<'ink, 'ctx> {
    fn save(&mut self, msg: crate::message::message::Message) {
        self.msg.add_message(msg)
    }
}

impl<'ink, 'ctx> CodeGen<'ink, 'ctx> {
    pub fn new(sess: Session, mir: &'ctx MIR, hir: &'ctx HIR, llvm_ctx: &'ink Context) -> Self {
        Self {
            sess,
            mir,
            hir,
            llvm_ctx,
            msg: MessageStorage::default(),
        }
    }

    fn codegen(&mut self) {
        let ctx = CodeGenCtx {
            sess: &self.sess,
            mir: self.mir,
            hir: self.hir,
            llvm_ctx: self.llvm_ctx,
        };

        let function_map = FunctionsCodeGen::new(ctx).gen_functions();

        // TODO
        // for (def_id, _func) in function_map.iter_enumerated_flat() {
        //     BodyCodeGen::new(ctx, def_id, &function_map).gen_body();
        // }
    }
}

impl<'ink, 'ctx> Stage<()> for CodeGen<'ink, 'ctx> {
    fn run(mut self) -> StageOutput<()> {
        self.codegen();
        // FIXME: Rewrite stages to error propagation model
        StageOutput::new(self.sess, (), self.msg)
    }
}
