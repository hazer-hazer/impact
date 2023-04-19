use std::fmt::Display;

use crate::{
    cli::command::{PPStages, StageName},
    session::InterruptionReason,
};

#[derive(Clone)]
pub struct Config {
    // PP //
    pp_stages: PPStages,
    pp_ast_ids: bool,

    // Debug and in-source testing //
    compilation_depth: StageName,
    expected_output: Option<String>,
    interruption_reason: Option<InterruptionReason>,
    verbose_messages: bool,
    parser_debug: bool,
}

impl Config {
    pub fn check_pp_stage(&self, stage: StageName) -> bool {
        match &self.pp_stages {
            PPStages::None => false,
            PPStages::Some(stages) => stages.contains(&stage),
            PPStages::All => true,
        }
    }

    pub fn pp_ast_ids(&self) -> bool {
        self.pp_ast_ids
    }

    pub fn expected_output(&self) -> Option<&String> {
        self.expected_output.as_ref()
    }

    pub fn interruption_reason(&self) -> Option<&InterruptionReason> {
        self.interruption_reason.as_ref()
    }

    pub fn verbose_messages(&self) -> bool {
        self.verbose_messages
    }

    pub fn parser_debug(&self) -> bool {
        self.parser_debug
    }

    pub fn compilation_depth(&self) -> StageName {
        self.compilation_depth
    }
}

impl Display for Config {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "compilation_depth='{}', pp_stages='{}', expected_output='{}', interruption_reason='{}', verbose_messages={}, parser_debug={}, pp_ast_ids={}",
            self.compilation_depth,
            self.pp_stages,
            self.expected_output.as_ref().map_or("none", |o| o.as_str()),
            self.interruption_reason.as_ref().map_or("none".to_string(), ToString::to_string),
            if self.verbose_messages {"yes"} else {"no"},
            if self.parser_debug {"yes"} else {"no"},
            if self.pp_ast_ids {"yes"} else {"no"},
        )
    }
}

pub const DEFAULT_COMPILATION_DEPTH: StageName = StageName::Unset;
pub const DEFAULT_PP_STAGES: PPStages = PPStages::None;
pub const DEFAULT_EXPECTED_OUTPUT: Option<String> = None;

#[derive(Default)]
pub struct ConfigBuilder {
    pp_stages: Option<PPStages>,
    pp_ast_ids: bool,

    compilation_depth: Option<StageName>,
    expected_output: Option<String>,
    interruption_reason: Option<InterruptionReason>,
    verbose_messages: bool,
    parser_debug: bool,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        Self {
            pp_stages: None,
            pp_ast_ids: false,

            compilation_depth: None,
            expected_output: None,
            interruption_reason: None,
            verbose_messages: false,
            parser_debug: false,
        }
    }

    pub fn emit(self) -> Config {
        Config {
            pp_ast_ids: self.pp_ast_ids,
            pp_stages: self.pp_stages.unwrap_or(DEFAULT_PP_STAGES),

            compilation_depth: self.compilation_depth.unwrap_or(DEFAULT_COMPILATION_DEPTH),
            expected_output: self.expected_output,
            interruption_reason: self.interruption_reason,
            verbose_messages: self.verbose_messages,
            parser_debug: self.parser_debug,
        }
    }

    pub fn pp_stages(mut self, pp_stages: PPStages) -> Self {
        self.pp_stages = Some(pp_stages);
        self
    }

    pub fn pp_ast_ids(mut self, pp_ast_ids: bool) -> Self {
        self.pp_ast_ids = pp_ast_ids;
        self
    }

    pub fn compilation_depth(mut self, compilation_depth: StageName) -> Self {
        self.compilation_depth = Some(compilation_depth);
        self
    }

    pub fn expected_output(mut self, expected_output: String) -> Self {
        self.expected_output = Some(expected_output);
        self
    }

    pub fn interruption_reason(mut self, interruption_reason: InterruptionReason) -> Self {
        self.interruption_reason = Some(interruption_reason);
        self
    }

    pub fn verbose_messages(mut self, verbose_messages: bool) -> Self {
        self.verbose_messages = verbose_messages;
        self
    }

    pub fn parser_debug(mut self, parser_debug: bool) -> Self {
        self.parser_debug = parser_debug;
        self
    }
}
