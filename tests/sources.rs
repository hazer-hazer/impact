use core::panic;
use std::{
    env,
    fs::{read_dir, File},
    io::{self, BufRead, BufReader},
    path::Path,
};

use impact::{
    cli::color::Colorize,
    config::config::{Config, PPStages, StageName},
    interface::interface::{Interface, InterruptionReason},
    message::message::MessageKind,
    session::Source,
};

const OPTIONS_LINE_BEGIN: &str = "//>";
const DEFAULT_CONFIG: Config = Config {
    compilation_depth: StageName::Unset,
    pp_stages: PPStages::None,
};

// TODO: Move this to `cli` module
enum ConfigOptionKind {
    StopAt(StageName),
    PPStages(PPStages),
}

impl ConfigOptionKind {
    fn stop_at(stage_name: &str) -> Self {
        Self::StopAt(StageName::from_str(stage_name))
    }

    fn pp_stages(pp_stages: PPStages) -> Self {
        Self::PPStages(pp_stages)
    }
}

struct ConfigOption {
    kind: ConfigOptionKind,
}

impl ConfigOption {
    fn new(kind: ConfigOptionKind) -> Self {
        Self { kind }
    }
}

struct ConfigOptionBuilder<'a> {
    name: &'a str,
    args: Vec<&'a str>,
}

impl<'a> ConfigOptionBuilder<'a> {
    fn new(name: &'a str) -> Self {
        Self {
            name,
            args: Default::default(),
        }
    }

    fn arg(&mut self, arg: &'a str) -> &mut Self {
        self.args.push(arg);
        self
    }

    fn emit(self) -> ConfigOption {
        ConfigOption::new(match self.name {
            "stop-at" => {
                assert!(
                    self.args.len() == 1,
                    "Invalid count of arguments given for test option `stop_at`"
                );
                ConfigOptionKind::stop_at(self.args[0])
            },
            "pp" => {
                assert!(self.args.len() >= 1);
                ConfigOptionKind::pp_stages(if self.args.len() > 1 {
                    PPStages::Some(
                        self.args
                            .iter()
                            .map(|stage| StageName::from_str(stage))
                            .collect::<Vec<_>>(),
                    )
                } else {
                    match self.args[0] {
                        "[all]" => PPStages::All,
                        "[none]" => PPStages::None,
                        _ => PPStages::Some(vec![StageName::from_str(self.args[0])]),
                    }
                })
            },
            _ => panic!("Unknown test option `{}`", self.name),
        })
    }
}

struct Test {
    source: Source,
    config: Config,
}

impl Test {
    fn new(source: Source, config: Config) -> Self {
        Self { source, config }
    }
}

fn config_from_options(options: Vec<ConfigOption>) -> Config {
    let mut config = DEFAULT_CONFIG;
    for opt in options {
        match opt.kind {
            ConfigOptionKind::StopAt(stage) => config.compilation_depth = stage,
            ConfigOptionKind::PPStages(pp_stages) => config.pp_stages = pp_stages,
        }
    }
    config
}

fn parse_test(path: &Path) -> io::Result<Test> {
    assert!(path.is_file());

    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut content = String::default();
    let mut options = Vec::default();

    for line in reader.lines() {
        let line = line?;
        content.push_str(&format!("{}\n", line));

        if line.starts_with(OPTIONS_LINE_BEGIN) {
            options.extend(
                line[OPTIONS_LINE_BEGIN.len()..]
                    .split(" ")
                    .filter(|arg| !arg.trim().is_empty())
                    .fold(Vec::default(), |mut opts, str| {
                        if str.starts_with("--") {
                            opts.push(ConfigOptionBuilder::new(&str[2..]));
                        } else if opts.is_empty() {
                            panic!("Unexpected option argument `{}` without option", str);
                        } else {
                            opts.last_mut().unwrap().arg(str);
                        }
                        opts
                    })
                    .into_iter()
                    .map(|opt| opt.emit())
                    .collect::<Vec<ConfigOption>>(),
            );
        }
    }

    Ok(Test::new(
        Source::new(path.to_str().unwrap().to_string(), content),
        config_from_options(options),
    ))
}

fn parse_all_tests(dir: &Path, cb: fn(Test)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                parse_all_tests(&path, cb)?;
            } else {
                cb(parse_test(&path)?);
            }
        }
    }

    Ok(())
}

#[test]
fn test_sources() -> io::Result<()> {
    let sources_path = Path::new("tests/sources");

    parse_all_tests(&sources_path, |test: Test| {
        println!("Running test `{}`", test.source.filename());

        let interface = Interface::new(test.config);
        let result = interface.compile_single_source(test.source);

        if let Err(err) = result {
            match err {
                InterruptionReason::ConfiguredStop => {
                    println!("Stop due to configured compilation depth")
                },
                InterruptionReason::Error(err) => {
                    println!("{}", err.fg_color(MessageKind::Error.color()))
                },
            }
        }
    })?;

    Ok(())
}
