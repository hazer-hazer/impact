use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

use inkwell::{
    context::Context,
    targets::{FileType, InitializationConfig, Target, TargetMachine},
};

use crate::{
    cli::{color::Colorize, verbose},
    hir::HIR,
    message::message::{MessageHolder, MessageStorage, MessagesResult},
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

    fn codegen(&mut self) -> MessagesResult<()> {
        let _main_func = self
            .sess
            .def_table
            .main_func()
            .map_err(|err| err.emit(self));

        let llvm_module = self.llvm_ctx.create_module("kek");
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        dbg!(&target_triple);
        llvm_module.set_triple(&target_triple);

        let ctx = CodeGenCtx {
            sess: &self.sess,
            mir: self.mir,
            hir: self.hir,
            llvm_ctx: self.llvm_ctx,
            llvm_module: &llvm_module,
        };

        let function_map = FunctionsCodeGen::new(ctx).gen_functions()?;
        let value_map = ValueCodeGen::new(ctx, &function_map).gen_values();

        for (def_id, inst) in function_map.iter_internal() {
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

        let path = Path::new("output/kek").with_extension("o");
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "",
                "",
                inkwell::OptimizationLevel::None,
                inkwell::targets::RelocMode::PIC,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();

        target_machine
            .write_to_file(&llvm_module, FileType::Object, &path)
            .unwrap();

        let mut child = Command::new("gcc")
            .arg(path.to_string_lossy().as_ref())
            .arg("-Wno-everything")
            .arg("-O0")
            .arg("-lm")
            .arg(format!(
                "-o{}",
                PathBuf::from("output/kek")
                    .with_extension("")
                    .to_string_lossy()
            ))
            .spawn()
            .unwrap();

        child.wait().unwrap();

        let mut file = File::create(path.with_extension("ll")).unwrap();
        file.write_all(llvm_module.to_string().as_bytes()).unwrap();

        Ok(())
    }
}

impl<'ink, 'ctx> Stage<()> for CodeGen<'ink, 'ctx> {
    fn run(mut self) -> StageOutput<()> {
        let result = self.codegen();
        if let Err(msgs) = result {
            self.msg.merge(msgs);
        }
        // FIXME: Rewrite stages to error propagation model
        StageOutput::new(self.sess, (), self.msg)
    }
}
