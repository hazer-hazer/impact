use inkwell::{builder::Builder, context::Context, module::Module};

use crate::session::Session;

pub struct CodeGenCtx<'ctx> {
    sess: &'ctx Session,
    llvm_ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> CodeGenCtx<'ctx> {}
