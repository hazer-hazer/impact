use crate::{
    ast::visitor::AstVisitor,
    cli::verbose,
    config::config::{Config, StageName},
    hir::visitor::HirVisitor,
    lower::Lower,
    parser::{lexer::Lexer, parser::Parser},
    pp::AstLikePP,
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
        verbose!(format!("=== Lexing ==="));
        let stage = StageName::Lexer;

        let (tokens, sess) =
            Lexer::new(sess.source_map.add_source(source), sess).run_and_emit(true)?;

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
        verbose!(format!("=== Parsing ==="));
        let stage = StageName::Parser;
        let (ast, sess) = Parser::new(sess, tokens).run_and_emit(true)?;

        if self.config.check_pp_stage(stage) {
            let mut pp = AstLikePP::new(&sess);
            println!("Printing AST after parsing\n{}", pp.visit_ast(&ast));
        }

        self.should_stop(stage)?;

        // Lowering //
        let stage = StageName::Lower;
        let (hir, sess) = Lower::new(sess, &ast).run_and_emit(true)?;

        if self.config.check_pp_stage(stage) {
            let mut pp = AstLikePP::new(&sess);
            println!("Printing HIR after parsing\n{}", pp.visit_hir(&hir));
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
