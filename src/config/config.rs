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

impl StageName {
    pub fn from_str(name: &str) -> Self {
        match name {
            "lexer" => Self::Lexer,
            "parser" => Self::Parser,
            "ast_validation" => Self::AstValidation,
            "def_collect" => Self::DefCollect,
            "nameres" => Self::NameRes,
            "lower" => Self::Lower,
            "typeck" => Self::Typeck,
            _ => panic!("Invalid stage name `{}`", name),
        }
    }
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
    pub expected_output: Option<String>,
}

impl Config {
    pub fn check_pp_stage(&self, stage: StageName) -> bool {
        match &self.pp_stages {
            PPStages::None => false,
            PPStages::Some(stages) => stages.contains(&stage),
            PPStages::All => true,
        }
    }

    pub fn expected_output(&self) -> Option<&String> {
        self.expected_output.as_ref()
    }
}

pub const DEFAULT_COMPILATION_DEPTH: StageName = StageName::Unset;
pub const DEFAULT_PP_STAGES: PPStages = PPStages::None;
pub const DEFAULT_EXPECTED_OUTPUT: Option<String> = None;

#[derive(Default)]
pub struct ConfigBuilder {
    compilation_depth: Option<StageName>,
    pp_stages: Option<PPStages>,
    expected_output: Option<String>,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn compilation_depth(mut self, compilation_depth: StageName) -> Self {
        self.compilation_depth = Some(compilation_depth);
        self
    }

    pub fn pp_stages(mut self, pp_stages: PPStages) -> Self {
        self.pp_stages = Some(pp_stages);
        self
    }

    pub fn expected_output(mut self, expected_output: String) -> Self {
        self.expected_output = Some(expected_output);
        self
    }

    pub fn emit(self) -> Config {
        Config {
            compilation_depth: self.compilation_depth.unwrap_or(DEFAULT_COMPILATION_DEPTH),
            pp_stages: self.pp_stages.unwrap_or(DEFAULT_PP_STAGES),
            expected_output: self.expected_output,
        }
    }
}
