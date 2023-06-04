use inkwell::context::Context;

use crate::{
    ast::{
        map::{AstMapFiller, MappedAst},
        validator::AstValidator,
        visitor::AstVisitor,
        NodeId,
    },
    cli::{command::StageName, verbose},
    codegen::codegen::CodeGen,
    config::config::Config,
    hir::visitor::HirVisitor,
    interface::writer::outln,
    lower::Lower,
    mir::build::BuildFullMir,
    parser::{lexer::Lexer, parser::Parser},
    pp::{defs::DefPrinter, hir::HirPP, mir::MirPrinter, AstLikePP, AstPPMode},
    resolve::{
        collect::DefCollector,
        def::{DefId, ModuleId},
        resolve::NameResolver,
    },
    session::{InterruptResult, InterruptionReason, Session, Stage, StageResultImpl},
    span::source::Source,
    typeck::Typecker,
};

pub struct Interface {
    config: Config,
}

impl<'ast> Interface {
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    pub fn compile_single_source(self, source: Source) -> InterruptResult {
        let mut sess = Session::new(self.config.clone());

        // Debug info //
        verbose!("== Identifiers format ==");
        verbose!("NodeId: {}", NodeId::new(0));
        verbose!("DefId: {}", DefId::new(0));
        verbose!("ModuleId::Block: {}", ModuleId::Block(NodeId::new(0)));
        verbose!("ModuleId::Module: {}", ModuleId::Def(DefId::new(0)));

        // Lexing //
        verbose!("=== Lexing ===");
        let stage = StageName::Lexer;

        let source_id = sess.source_map.add_source(source);

        let (tokens, mut sess) = Lexer::new(source_id, sess).run_and_emit()?;

        if cfg!(feature = "pp_lines") {
            outln!(
                dbg,
                sess.writer,
                "=== SOURCE LINES ===\n{}\nPositions: {:?}\n",
                sess.source_map
                    .get_source(source_id)
                    .get_lines()
                    .iter()
                    .enumerate()
                    .map(|(index, line)| format!("   {} | {}", index + 1, line))
                    .collect::<Vec<_>>()
                    .join("\n"),
                sess.source_map.get_source(source_id).lines_positions()
            );
        }

        if sess.config().check_pp_stage(stage) {
            outln!(dbg, sess.writer, "Tokens: {}", tokens);
        }

        let sess = self.should_stop(sess, stage)?;

        // Parsing //
        verbose!("=== Parsing ===");
        let stage = StageName::Parser;

        let mut parse_result = Parser::new(sess, tokens).run().recover()?;

        if parse_result.ctx().config().check_pp_stage(stage) {
            let mut pp = AstLikePP::new(parse_result.ctx(), AstPPMode::Normal);
            pp.visit_ast(&parse_result.data());
            let ast = pp.get_string();
            outln!(
                dbg,
                parse_result.ctx_mut().writer,
                "Printing AST after parsing\n{}",
                ast
            );
        }

        let (ast, sess) = parse_result.emit()?;

        let _mapped_ast = MappedAst::new(&ast, AstMapFiller::new().fill(&ast));

        let sess = self.should_stop(sess, stage)?;

        // AST Validation //
        verbose!("=== AST Validation ===");
        let stage = StageName::AstValidation;

        let (_, sess) = AstValidator::new(sess, &ast).run_and_emit()?;

        let sess = self.should_stop(sess, stage)?;

        // Def collection //
        verbose!("=== Definition collection ===");
        let stage = StageName::DefCollect;

        let (_, mut sess) = DefCollector::new(sess, &ast).run_and_emit()?;

        if sess.config().check_pp_stage(stage) {
            let mut pp = AstLikePP::new(&sess, AstPPMode::Normal);
            pp.pp_defs();
            let defs = pp.get_string();
            outln!(
                dbg,
                sess.writer,
                "Printing definitions after def collection\n{}",
                defs
            );
        }

        let sess = self.should_stop(sess, stage)?;

        // Name resolution //
        verbose!("=== Name resolution ===");
        let stage = StageName::NameRes;

        let mut name_res_result = NameResolver::new(sess, &ast).run().recover()?;

        if name_res_result.ctx().config().check_pp_stage(stage) {
            let mut pp = AstLikePP::new(name_res_result.ctx(), AstPPMode::Normal);
            pp.pp_defs();
            let defs = pp.get_string();
            outln!(
                dbg,
                name_res_result.ctx_mut().writer,
                "Printing definitions after name resolution\n{}",
                defs
            );
        }

        if name_res_result.ctx().config().check_pp_stage(stage) {
            let mut pp = AstLikePP::new(name_res_result.ctx(), AstPPMode::NameHighlighter);
            pp.visit_ast(&ast);
            let ast = pp.get_string();
            outln!(dbg, name_res_result.ctx_mut().writer, "Printing AST after name resolution (resolved names are marked with the same color)\n{}", ast);
        }

        let (_, sess) = name_res_result.emit()?;

        let sess = self.should_stop(sess, stage)?;

        // Lowering //
        verbose!("=== Lowering ===");
        let stage = StageName::Lower;

        let (hir, mut sess) = Lower::new(sess, &ast).run_and_emit()?;

        if sess.config().check_pp_stage(stage) {
            let mut pp = HirPP::new(&sess, AstPPMode::Normal);
            pp.visit_hir(&hir);
            let hir = pp.pp.get_string();
            outln!(dbg, sess.writer, "Printing HIR\n{}", hir);
        }

        let sess = self.should_stop(sess, stage)?;

        // Typeck //
        verbose!("=== Type checking ===");
        let stage = StageName::Typeck;
        let mut typeck_result = Typecker::new(sess, &hir).run().recover()?;

        if typeck_result.ctx().config().check_pp_stage(stage) {
            let mut pp = HirPP::new(typeck_result.ctx(), AstPPMode::TyAnno);
            pp.visit_hir(&hir);
            let hir = pp.pp.get_string();
            outln!(
                dbg,
                typeck_result.ctx_mut().writer,
                "Printing HIR with type annotations\n{}",
                hir
            );
        }

        let (_, sess) = typeck_result.emit()?;

        let sess = self.should_stop(sess, stage)?;

        // MIR Construction //
        verbose!("=== MIR Construction ===");
        let stage = StageName::MirConstruction;

        let (mir, mut sess) = BuildFullMir::new(sess, &hir).run_and_emit()?;

        if sess.config().check_pp_stage(stage) {
            let mut pp = MirPrinter::new(&sess, &hir, &mir);
            pp.visit_hir(&hir);
            let mir = pp.pp.get_string();
            outln!(dbg, sess.writer, "== MIR ==\n{}", mir);
        }

        let sess = self.should_stop(sess, stage)?;

        // Codegen //
        verbose!("=== Codegen ===");
        let stage = StageName::Codegen;

        let llvm_ctx = Context::create();
        let (_, sess) = CodeGen::new(sess, &hir, &mir, &llvm_ctx).run_and_emit()?;

        let sess = self.should_stop(sess, stage)?;

        Ok(sess)
    }

    fn should_stop(&self, sess: Session, stage: StageName) -> InterruptResult {
        if self.config.compilation_depth() <= stage {
            verbose!(
                "Should stop on {} as {} is configured",
                stage,
                self.config.compilation_depth()
            );
            Err((InterruptionReason::ConfiguredStop, sess))
        } else {
            Ok(sess)
        }
    }

    pub fn config(&self) -> &Config {
        &self.config
    }
}
