use crate::{
    ast::{validator::AstValidator, visitor::AstVisitor},
    cli::verbose,
    config::config::{Config, StageName},
    hir::visitor::HirVisitor,
    lower::Lower,
    parser::{lexer::Lexer, parser::Parser},
    pp::{defs::DefPrinter, AstLikePP, AstPPMode},
    resolve::{collect::DefCollector, resolve::NameResolver},
    session::{Session, Source, Stage},
};

pub struct Interface {
    config: Config,
}

type InterruptResult = Result<(), String>;

impl Interface {
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    pub fn compile_single_source(self, source: Source) -> InterruptResult {
        let mut sess = Session::new(self.config.clone());

        // Lexing //
        verbose!("=== Lexing ===");
        let stage = StageName::Lexer;

        let source_id = sess.source_map.add_source(source);

        let (tokens, sess) = Lexer::new(source_id, sess).run_and_emit(true)?;

        if cfg!(feature = "pp_lines") {
            println!(
                "=== SOURCE LINES ===\n{}\nPosition: {:?}\n",
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

        if self.config.check_pp_stage(stage) {
            println!(
                "Printing tokens after lexing\n{}",
                tokens
                    .0
                    .iter()
                    .map(|t| format!("{:?}", t))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        }

        self.should_stop(stage)?;

        // Parsing //
        verbose!("=== Parsing ===");
        let stage = StageName::Parser;
        let parse_result = Parser::new(sess, tokens).run();

        if self.config.check_pp_stage(stage) {
            let mut pp = AstLikePP::new(parse_result.sess(), AstPPMode::Normal);
            pp.visit_ast(parse_result.data());
            println!("Printing AST after parsing\n{}", pp.get_string());
        }

        let (ast, sess) = parse_result.emit(true)?;

        self.should_stop(stage)?;

        // AST Validation //
        verbose!("=== AST Validation ===");
        let stage = StageName::AstValidation;
        let (_, sess) = AstValidator::new(sess, &ast).run_and_emit(true)?;

        self.should_stop(stage)?;

        // Def collection //
        verbose!("=== Definition collection ===");
        let stage = StageName::DefCollect;

        let (_, sess) = DefCollector::new(sess, &ast).run_and_emit(true)?;

        if self.config.check_pp_stage(stage) {
            let mut pp = AstLikePP::new(&sess, AstPPMode::Normal);
            pp.pp_defs();
            println!("{}", pp.get_string());
        }

        self.should_stop(stage)?;

        // Name resolution //
        verbose!("=== Name resolution ===");
        let stage = StageName::NameRes;

        let (_, sess) = NameResolver::new(sess, &ast).run_and_emit(true)?;

        if self.config.check_pp_stage(stage) {
            let mut pp = AstLikePP::new(&sess, AstPPMode::NameHighlighter);
            pp.visit_ast(&ast);
            println!("{}", pp.get_string());
        }

        self.should_stop(stage)?;

        // Lowering //
        verbose!("=== Lowering ===");
        let stage = StageName::Lower;
        let (hir, sess) = Lower::new(sess, &ast).run_and_emit(true)?;

        if self.config.check_pp_stage(stage) {
            let mut pp = AstLikePP::new(&sess, AstPPMode::Normal);
            pp.visit_hir(&hir);
            println!("Printing HIR after parsing\n{}", pp.get_string());
        }

        self.should_stop(stage)?;

        Ok(())
    }

    fn should_stop(&self, stage: StageName) -> InterruptResult {
        if self.config.compilation_depth <= stage {
            Err(format!(
                "Compilation stopped due to configured compilation depth {}",
                self.config.compilation_depth
            ))
        } else {
            Ok(())
        }
    }
}
