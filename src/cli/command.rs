use std::fmt::Display;


use clap::{arg, command, ValueEnum};
use clap::{Parser};

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, ValueEnum)]
pub enum StageName {
    #[value(name = "lexer")]
    Lexer,
    #[value(name = "parser")]
    Parser,
    #[value(name = "ast-check")]
    AstValidation,
    #[value(name = "def-collect")]
    DefCollect,
    #[value(name = "resolve")]
    NameRes,
    #[value(name = "ast2hir")]
    Lower,
    #[value(name = "typeck")]
    Typeck,
    #[value(name = "hir2mir")]
    MirConstruction,
    #[value(name = "codegen")]
    Codegen,
    #[value(skip)]
    Unset,
    #[value(name = "all")]
    Any,
}

impl TryFrom<&str> for StageName {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "lexer" => Ok(Self::Lexer),
            "parser" => Ok(Self::Parser),
            "ast-check" => Ok(Self::AstValidation),
            "def-collect" => Ok(Self::DefCollect),
            "resolve" => Ok(Self::NameRes),
            "ast2hir" => Ok(Self::Lower),
            "typeck" => Ok(Self::Typeck),
            "hir2mir" => Ok(Self::MirConstruction),
            "codegen" => Ok(Self::Codegen),
            "all" | "any" => Ok(Self::Any),
            _ => Err(()),
        }
    }
}

impl Display for StageName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                StageName::Lexer => "lexing",
                StageName::Parser => "parsing",
                StageName::AstValidation => "ast validation",
                StageName::DefCollect => "definition collection",
                StageName::NameRes => "name resolution",
                StageName::Lower => "lowering",
                StageName::Typeck => "type checking",
                StageName::MirConstruction => "mir construction",
                StageName::Codegen => "codegen",
                StageName::Any => "*",
                StageName::Unset => "[unknown]",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum PPStages {
    None,
    Some(Vec<StageName>),
    All,
}

// impl From<ArgMatches> for PPStages {
//     fn from(value: ArgMatches) -> Self {
//         match value.get_many("pp-stages") {
//             Some(stages) => {

//             },
//             None => Self::None,
//         }
//     }
// }

impl Display for PPStages {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PPStages::None => write!(f, "none"),
            PPStages::Some(stages) => write!(
                f,
                "{}",
                stages
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            PPStages::All => write!(f, "all"),
        }
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
pub struct Args {
    /// PP Section

    /// Pretty-print compilation stages
    #[arg(long, help_heading = "Debug options")]
    pub pp_stages: Option<Vec<StageName>>,

    /// Print IDs in AST-like structures (AST, HIR, etc.)
    #[arg(long, default_value_t = false, help_heading = "Debug options")]
    pub pp_ast_ids: bool,

    /// Compile until specified stage
    #[arg(long, help_heading = "Debug options")]
    pub compilation_depth: Option<StageName>,

    // TODO: File with expected output
    /// Assert that compiler debug info equals to the given string
    #[arg(long, help_heading = "Debug options")]
    pub expected_output: Option<String>,

    // interruption_reason
    /// Emit verbose debug messages
    #[arg(long, default_value_t = false, help_heading = "Debug options")]
    pub verbose_messages: bool,

    /// Prints tree view of parser entries
    #[arg(long, default_value_t = false, help_heading = "Debug options")]
    pub parser_debug: bool,
}

#[cfg(test)]
mod tests {
    use crate::cli::command::StageName;

    #[test]
    pub fn stage_name_ordering() {
        assert!(StageName::Lexer < StageName::Parser);
        assert!(StageName::Parser < StageName::AstValidation);
        assert!(StageName::AstValidation < StageName::DefCollect);
        assert!(StageName::DefCollect < StageName::NameRes);
        assert!(StageName::NameRes < StageName::Lower);
        assert!(StageName::Lower < StageName::Typeck);
        assert!(StageName::Typeck < StageName::MirConstruction);
        assert!(StageName::MirConstruction < StageName::Codegen);
    }

    #[test]
    pub fn stage_name_ne() {
        assert_ne!(StageName::Lexer, StageName::Parser);
        assert_ne!(StageName::Parser, StageName::AstValidation);
        assert_ne!(StageName::AstValidation, StageName::DefCollect);
        assert_ne!(StageName::DefCollect, StageName::NameRes);
        assert_ne!(StageName::NameRes, StageName::Lower);
        assert_ne!(StageName::Lower, StageName::Typeck);
        assert_ne!(StageName::Typeck, StageName::MirConstruction);
        assert_ne!(StageName::MirConstruction, StageName::Codegen);
    }
}
