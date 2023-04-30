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

use super::{
    body::BodyCodeGen,
    ctx::CodeGenCtx,
    func::{FuncInstance, FunctionsCodeGen},
    value::ValueCodeGen,
};
use crate::{
    cli::{color::Colorize, verbose},
    hir::HIR,
    message::message::{impl_message_holder, MessageHolder, MessageStorage},
    mir::MIR,
    session::{stage_result, Session, Stage, StageResult, StageResultImpl},
};

pub struct CodeGen<'ink, 'ctx> {
    mir: &'ctx MIR,
    hir: &'ctx HIR,

    llvm_ctx: &'ink Context,

    sess: Session,
    msg: MessageStorage,
}

impl_message_holder!(CodeGen<'ink, 'ctx>);

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

    fn codegen(mut self) -> StageResult<(), CodeGenCtx<'ink, 'ctx>> {
        let _main_func = self
            .sess
            .def_table
            .main_func()
            .map_err(|err| err.emit(&mut self));

        let llvm_module = self.llvm_ctx.create_module("kek");
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        dbg!(&target_triple);
        llvm_module.set_triple(&target_triple);

        let ctx = CodeGenCtx {
            sess: self.sess,
            mir: self.mir,
            hir: self.hir,
            llvm_ctx: self.llvm_ctx,
            llvm_module,
        };

        let (function_map, ctx) = FunctionsCodeGen::new(ctx).run()?.merged(&mut self.msg);

        let (value_map, ctx) = ValueCodeGen::new(ctx, &function_map)
            .run()?
            .merged(&mut self.msg);

        let ctx = function_map
            .iter_internal()
            .try_fold(ctx, |ctx, (def_id, inst)| match inst {
                &FuncInstance::Mono(ty, func) => {
                    let ((), ctx) =
                        BodyCodeGen::new(ctx, def_id, ty, func, &function_map, &value_map)
                            .run()?
                            .merged(&mut self.msg);
                    Ok(ctx)
                },
                FuncInstance::Poly(poly) => {
                    poly.iter_enumerated_flat()
                        .try_fold(ctx, |ctx, (_, &(ty, func))| {
                            let ((), ctx) =
                                BodyCodeGen::new(ctx, def_id, ty, func, &function_map, &value_map)
                                    .run()?
                                    .merged(&mut self.msg);
                            Ok(ctx)
                        })
                },
            })?;

        verbose!("LLVM Module:\n{}", ctx.llvm_module.to_string());

        ctx.llvm_module
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
            .write_to_file(&ctx.llvm_module, FileType::Object, &path)
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
        file.write_all(ctx.llvm_module.to_string().as_bytes())
            .unwrap();

        stage_result(ctx, (), self.msg)
    }
}

impl<'ink, 'ctx> Stage<()> for CodeGen<'ink, 'ctx> {
    fn run(self) -> StageResult<()> {
        self.codegen().map_ctx(|ctx| ctx.sess)
    }
}
