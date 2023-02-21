use std::fmt::Display;

use inkwell::context::Context;

use crate::{
    ast::{validator::AstValidator, visitor::AstVisitor, AstMapFiller, MappedAst, NodeId},
    cli::verbose,
    codegen::codegen::CodeGen,
    config::config::{Config, StageName},
    hir::visitor::HirVisitor,
    interface::writer::outln,
    lower::Lower,
    parser::{lexer::Lexer, parser::Parser},
    pp::{defs::DefPrinter, hir::HirPP, AstLikePP, AstPPMode},
    resolve::{
        collect::DefCollector,
        def::{DefId, ModuleId},
        resolve::NameResolver,
    },
    session::{Session, Source, Stage},
    typeck::Typecker,
};

pub struct Interface {
    config: Config,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterruptionReason {
    ConfiguredStop,
    ErrorMessage,
}

impl InterruptionReason {
    pub fn from_str(str: &str) -> Self {
        match str {
            "configured" => Self::ConfiguredStop,
            "error" => Self::ErrorMessage,
            _ => panic!("Invalid interruption reason name"),
        }
    }
}

impl Display for InterruptionReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InterruptionReason::ConfiguredStop => "configured",
                InterruptionReason::ErrorMessage => "error",
            }
        )
    }
}

pub type InterruptResult<'ast> = Result<Session, (InterruptionReason, Session)>;
pub type UnitInterruptResult<'ast> = Result<Session, (InterruptionReason, Session)>;

impl<'ast> Interface {
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    pub fn compile_single_source(self, source: Source) -> InterruptResult<'ast> {
        let mut sess = Session::new(self.config.clone());

        // Debug info //
        verbose!("== Identifiers format ==");
        verbose!("NodeId: {}", NodeId::new(0));
        verbose!("DefId: {}", DefId::new(0));
        verbose!("ModuleId::Block: {}", ModuleId::Block(NodeId::new(0)));
        verbose!("ModuleId::Module: {}", ModuleId::Module(DefId::new(0)));

        // Lexing //
        verbose!("=== Lexing ===");
        let stage = StageName::Lexer;

        let source_id = sess.source_map.add_source(source);

        let (tokens, mut sess) = Lexer::new(source_id, sess).run_and_emit(true)?;

        if cfg!(feature = "pp_lines") {
            outln!(
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
            outln!(sess.writer, "Tokens: {}", tokens);
        }

        let sess = self.should_stop(sess, stage)?;

        // Parsing //
        verbose!("=== Parsing ===");
        let stage = StageName::Parser;

        let mut parse_result = Parser::new(sess, tokens).run();

        if parse_result.sess().config().check_pp_stage(stage) {
            let mut pp = AstLikePP::new(parse_result.sess(), AstPPMode::Normal);
            pp.visit_ast(&parse_result.data());
            let ast = pp.get_string();
            outln!(
                parse_result.sess_mut().writer,
                "Printing AST after parsing\n{}",
                ast
            );
        }

        let (ast, sess) = parse_result.emit(true)?;

        let _mapped_ast = MappedAst::new(&ast, AstMapFiller::new().fill(&ast));

        let sess = self.should_stop(sess, stage)?;

        // AST Validation //
        verbose!("=== AST Validation ===");
        let stage = StageName::AstValidation;

        let (_, sess) = AstValidator::new(sess, &ast).run_and_emit(true)?;

        let sess = self.should_stop(sess, stage)?;

        // Def collection //
        verbose!("=== Definition collection ===");
        let stage = StageName::DefCollect;

        let (_, mut sess) = DefCollector::new(sess, &ast).run_and_emit(true)?;

        if sess.config().check_pp_stage(stage) {
            let mut pp = AstLikePP::new(&sess, AstPPMode::Normal);
            pp.pp_defs();
            let defs = pp.get_string();
            outln!(
                sess.writer,
                "Printing definitions after def collection\n{}",
                defs
            );
        }

        let sess = self.should_stop(sess, stage)?;

        // Name resolution //
        verbose!("=== Name resolution ===");
        let stage = StageName::NameRes;

        let mut name_res_result = NameResolver::new(sess, &ast).run();

        if name_res_result.sess().config().check_pp_stage(stage) {
            let mut pp = AstLikePP::new(name_res_result.sess(), AstPPMode::Normal);
            pp.pp_defs();
            let defs = pp.get_string();
            outln!(
                name_res_result.sess_mut().writer,
                "Printing definitions after name resolution\n{}",
                defs
            );
        }

        if name_res_result.sess().config().check_pp_stage(stage) {
            let mut pp = AstLikePP::new(name_res_result.sess(), AstPPMode::NameHighlighter);
            pp.visit_ast(&ast);
            let ast = pp.get_string();
            outln!(name_res_result.sess_mut().writer, "Printing AST after name resolution (resolved names are marked with the same color)\n{}", ast);
        }

        let (_, sess) = name_res_result.emit(true)?;

        let sess = self.should_stop(sess, stage)?;

        // Lowering //
        verbose!("=== Lowering ===");
        let stage = StageName::Lower;

        let (hir, mut sess) = Lower::new(sess, &ast).run_and_emit(true)?;

        if sess.config().check_pp_stage(stage) {
            let mut pp = HirPP::new(&sess, AstPPMode::Normal);
            pp.visit_hir(&hir);
            let hir = pp.pp.get_string();
            outln!(sess.writer, "Printing HIR\n{}", hir);
        }

        let sess = self.should_stop(sess, stage)?;

        // Typeck //
        verbose!("=== Type checking ===");
        let stage = StageName::Typeck;
        let mut typeck_result = Typecker::new(sess, &hir).run();

        if typeck_result.sess().config().check_pp_stage(stage) {
            let mut pp = HirPP::new(typeck_result.sess(), AstPPMode::TyAnno);
            pp.visit_hir(&hir);
            let hir = pp.pp.get_string();
            outln!(
                typeck_result.sess_mut().writer,
                "Printing HIR with type annotations\n{}",
                hir
            );
        }

        let (_, sess) = typeck_result.emit(true)?;

        let sess = self.should_stop(sess, stage)?;

        // Codegen //
        verbose!("=== Codegen ===");
        let stage = StageName::Codegen;

        let ctx = Context::create();
        let (_module, sess) = CodeGen::new(sess, &hir, &ctx).run_and_emit(true)?;

        let sess = self.should_stop(sess, stage)?;

        Ok(sess)
    }

    fn should_stop<'a>(&self, sess: Session, stage: StageName) -> UnitInterruptResult<'a> {
        if self.config.compilation_depth() <= stage {
            Err((InterruptionReason::ConfiguredStop, sess))
        } else {
            Ok(sess)
        }
    }

    pub fn config(&self) -> &Config {
        &self.config
    }
}
