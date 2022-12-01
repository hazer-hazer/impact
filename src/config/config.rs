use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum StageName {
    Lexer,
    Parser,
    AstValidation,
    DefCollect,
    NameRes,
    Lower,
    Typeck,
    Unset,
}

#[derive(Clone, Debug)]
pub enum PPStages {
    None,
    Some(Vec<StageName>),
    All,
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
                StageName::Unset => "[unknown]",
            }
        )
    }
}

#[derive(Clone)]
pub struct Config {
    pub compilation_depth: StageName,
    pub pp_stages: PPStages,
}

impl Config {
    pub fn check_pp_stage(&self, stage: StageName) -> bool {
        match &self.pp_stages {
            PPStages::None => false,
            PPStages::Some(stages) => stages.contains(&stage),
            PPStages::All => true,
        }
    }
}
