use clap::error::ErrorKind;
use clap::{arg, command};
use clap::{ArgMatches, Error, Parser};

use crate::config::config::{Config, ConfigBuilder, PPStages, StageName};

impl TryFrom<ArgMatches> for PPStages {
    type Error = Error;

    fn try_from(value: ArgMatches) -> Result<PPStages, Error> {
        if let Some(mut stages) = value.get_many::<&str>("pp-stages") {
            if stages.any(|stage| stage == &"*") {
                Ok(Self::All)
            } else {
                let stages: Result<Vec<_>, _> =
                    stages.map(|&stage| StageName::try_from(stage)).collect();
                if let Ok(stages) = stages {
                    Ok(Self::Some(stages))
                } else {
                    Err(Error::raw(ErrorKind::InvalidValue, "Invalid stage name"))
                }
            }
        } else {
            Ok(Self::None)
        }
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// PP Section

    /// Pretty-print compilation stages
    #[arg(long, help_heading = "Debug options")]
    pp_stages: Option<Vec<StageName>>,

    /// Print IDs in AST-like structures (AST, HIR, etc.)
    #[arg(long, default_value_t = false, help_heading = "Debug options")]
    pp_ast_ids: bool,

    /// Compile until specified stage
    #[arg(long, help_heading = "Debug options")]
    compilation_depth: Option<StageName>,

    // TODO: File with expected output
    /// Assert that compiler debug info equals to the given string
    #[arg(long, help_heading = "Debug options")]
    expected_output: Option<String>,

    // interruption_reason
    /// Emit verbose debug messages
    #[arg(long, default_value_t = false, help_heading = "Debug options")]
    verbose_messages: bool,

    /// Prints tree view of parser entries
    #[arg(long, default_value_t = false, help_heading = "Debug options")]
    parser_debug: bool,
}

impl Into<Config> for Args {
    fn into(self) -> Config {
        let b = ConfigBuilder::new();

        let b = b
            .pp_stages(match self.pp_stages {
                Some(stages) => {
                    if stages.contains(&StageName::Any) {
                        PPStages::All
                    } else {
                        PPStages::Some(stages)
                    }
                },
                None => PPStages::None,
            })
            .pp_ast_ids(self.pp_ast_ids)
            .verbose_messages(self.verbose_messages)
            .parser_debug(self.parser_debug);

        let b = if let Some(compilation_depth) = self.compilation_depth {
            b.compilation_depth(compilation_depth)
        } else {
            b
        };

        let b = if let Some(expected_output) = self.expected_output {
            b.expected_output(expected_output)
        } else {
            b
        };

        b.emit()
    }
}

pub struct CLI {
    config: Config,
}

impl CLI {
    pub fn new() -> CLI {
        Self {
            config: Args::parse().into(),
        }
    }

    pub fn config(&self) -> &Config {
        &self.config
    }
}
