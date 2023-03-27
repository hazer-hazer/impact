use inkwell::{context::Context, module::Module, targets::TargetMachine};

use crate::{
    cli::{color::Colorize, verbose},
    hir::HIR,
    message::message::{MessageHolder, MessageStorage},
    mir::MIR,
    session::{Session, Stage, StageOutput},
};

use super::{
    body::BodyCodeGen,
    ctx::CodeGenCtx,
    func::{FuncInstance, FunctionsCodeGen},
    value::ValueCodeGen,
};

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

    fn codegen(&mut self) -> Module<'ink> {
        let llvm_module = self.llvm_ctx.create_module("kek");
        let target_triple = TargetMachine::get_default_triple();
        llvm_module.set_triple(&target_triple);

        let ctx = CodeGenCtx {
            sess: &self.sess,
            mir: self.mir,
            hir: self.hir,
            llvm_ctx: self.llvm_ctx,
            llvm_module: &llvm_module,
        };

        let function_map = FunctionsCodeGen::new(ctx).gen_functions();
        let value_map = ValueCodeGen::new(ctx, &function_map).gen_values();

        for (def_id, inst) in function_map.iter() {
            match inst {
                &FuncInstance::Mono(ty, func) => {
                    BodyCodeGen::new(ctx, def_id, ty, func, &function_map, &value_map).gen_body();
                },
                FuncInstance::Poly(poly) => {
                    poly.iter_enumerated_flat().for_each(|(_, &(ty, func))| {
                        BodyCodeGen::new(ctx, def_id, ty, func, &function_map, &value_map)
                            .gen_body();
                    })
                },
            }
        }

        verbose!("LLVM Module:\n{}", llvm_module.to_string());

        llvm_module
            .verify()
            .map_err(|err| {
                println!(
                    "LLVM Module is invalid:\n{}",
                    err.to_string()
                        .fg_color(crate::cli::color::Color::BrightRed)
                );
            })
            .unwrap();

        llvm_module
    }
}

impl<'ink, 'ctx> Stage<Module<'ink>> for CodeGen<'ink, 'ctx> {
    fn run(mut self) -> StageOutput<Module<'ink>> {
        let module = self.codegen();
        // FIXME: Rewrite stages to error propagation model
        StageOutput::new(self.sess, module, self.msg)
    }
}
