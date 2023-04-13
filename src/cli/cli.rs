use clap::{error::ErrorKind, ArgMatches, Error, Parser};

use super::command::{Args, PPStages, StageName};
use crate::config::config::{Config, ConfigBuilder};

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
