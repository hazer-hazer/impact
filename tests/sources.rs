use core::panic;
use std::{
    fs::{read_dir, read_to_string},
    io::{self},
    path::Path,
};

use impact::{
    cli::{
        color::{Color, Colorize},
        command::{PPStages, StageName},
    },
    config::config::{Config, ConfigBuilder},
    interface::interface::{Interface, InterruptionReason},
    span::source::Source,
};
use similar::{ChangeTag, TextDiff};

// TODO: Move this to `cli` module
#[derive(Debug)]
enum ConfigOptionKind {
    StopAt(StageName),
    PPStages(PPStages),
    ExpectedOutput(String),
    InterruptionReason(InterruptionReason),
}

impl ConfigOptionKind {
    fn stop_at(stage_name: &str) -> Self {
        Self::StopAt(StageName::try_from(stage_name).unwrap())
    }

    fn pp_stages(pp_stages: PPStages) -> Self {
        Self::PPStages(pp_stages)
    }
}

#[derive(Debug)]
struct ConfigOption {
    kind: ConfigOptionKind,
}

impl ConfigOption {
    fn new(kind: ConfigOptionKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug)]
struct Comment {
    option: String,
    args: Vec<String>,
}

impl Comment {
    fn into_config_option(mut self) -> ConfigOption {
        ConfigOption::new(match self.option.as_str() {
            "stop-at" => {
                assert!(
                    self.args.len() == 1,
                    "Invalid count of arguments given for test option `stop_at`"
                );
                ConfigOptionKind::stop_at(self.args[0].as_str())
            },
            "pp" => {
                assert!(self.args.len() >= 1);
                ConfigOptionKind::pp_stages(if self.args.len() > 1 {
                    PPStages::Some(
                        self.args
                            .iter()
                            .map(|stage| StageName::try_from(stage.as_str()))
                            .collect::<Result<Vec<_>, _>>()
                            .unwrap(),
                    )
                } else {
                    match self.args[0].as_str() {
                        "all" => PPStages::All,
                        "none" => PPStages::None,
                        _ => {
                            PPStages::Some(
                                vec![StageName::try_from(self.args[0].as_str()).unwrap()],
                            )
                        },
                    }
                })
            },
            "expected-output" => {
                assert!(self.args.len() == 1);
                ConfigOptionKind::ExpectedOutput(self.args.remove(0))
            },
            "interruption-reason" => {
                assert!(self.args.len() == 1);
                ConfigOptionKind::InterruptionReason(InterruptionReason::from_str(&self.args[0]))
            },
            _ => panic!("Unknown test option `{}`", self.option),
        })
    }
}

#[derive(Default)]
struct CommentParser {
    source: String,
    index: usize,
    comments: Vec<Comment>,
}

impl CommentParser {
    fn eof(&self) -> bool {
        self.peek().is_none()
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.index)
    }

    fn lookup(&self, offset: usize) -> Option<char> {
        self.source.chars().nth(self.index + offset)
    }

    fn check(&self, pred: &[char]) -> bool {
        for i in 0..pred.len() {
            if let Some(ch) = self.lookup(i) {
                if ch != pred[i] {
                    return false;
                }
            }
        }
        true
    }

    fn any(&self, preds: &[char]) -> bool {
        for pred in preds {
            if let Some(ch) = self.peek() {
                if ch == *pred {
                    return true;
                }
            }
        }
        false
    }

    fn eat_until(&mut self, stop: &[char]) -> String {
        let mut s = String::new();

        while !self.check(stop) {
            s.push(self.peek().unwrap());
            self.advance(1);
        }

        assert!(self.advance_if(stop));

        s
    }

    fn advance(&mut self, offset: usize) {
        self.index += offset
    }

    fn advance_if(&mut self, pred: &[char]) -> bool {
        if self.check(pred) {
            self.advance(pred.len());
            true
        } else {
            false
        }
    }

    fn skip_ws(&mut self) -> bool {
        if let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance(1);
                return true;
            }
        }
        false
    }

    fn parse(mut self, source: String) -> (String, Vec<Comment>) {
        self.source = source;
        loop {
            if self.eof() {
                break;
            }

            let stop: Option<&[char]> = if self.advance_if(&['/', '/', '>']) {
                Some(&['\n'])
            } else if self.advance_if(&['/', '*', '>']) {
                // Note: No nested multiline comments
                Some(&['*', '/'])
            } else {
                None
            };

            if let Some(stop) = stop {
                self.skip_ws();

                loop {
                    if self.advance_if(stop) {
                        break;
                    }

                    if self.advance_if(&['-', '-']) {
                        let mut option = String::new();
                        while let Some(ch) = self.peek() {
                            if ch.is_alphanumeric() || ch == '-' {
                                option.push(ch);
                            } else {
                                break;
                            }
                            self.advance(1);
                        }

                        if option.is_empty() {
                            // TODO: CLI arguments (`--` separator)
                            panic!("Unexpected empty command `--{}`", option);
                        }

                        self.skip_ws();

                        if self.check(&['\'', '\'', '\'']) || self.check(&['"', '"', '"']) {
                            let quote = self.peek().unwrap();
                            self.advance(3);

                            let arg = self.eat_until(&[quote, quote, quote]);

                            self.comments.push(Comment {
                                option,
                                args: vec![arg.trim().to_string()],
                            });
                        } else if self.any(&['\'', '\"']) {
                            let quote = self.peek().unwrap();
                            self.advance(1);

                            let arg = self.eat_until(&[quote]);

                            self.comments.push(Comment {
                                option,
                                args: vec![arg],
                            });

                            continue;
                        } else {
                            let mut args = vec![];
                            while !self.eof() && !self.check(&['-', '-']) && !self.check(stop) {
                                let mut arg = String::new();
                                while !self.eof() && !self.check(stop) && !self.skip_ws() {
                                    arg.push(self.peek().unwrap());
                                    self.advance(1);
                                }
                                args.push(arg);
                            }
                            self.comments.push(Comment { option, args });
                        }
                    } else {
                        break;
                    }
                }
            } else {
                self.advance(1);
            }
        }

        (self.source, self.comments)
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
    let mut config = ConfigBuilder::new();
    for opt in options {
        config = match opt.kind {
            ConfigOptionKind::StopAt(stage) => config.compilation_depth(stage),
            ConfigOptionKind::PPStages(pp_stages) => config.pp_stages(pp_stages),
            ConfigOptionKind::ExpectedOutput(output) => config.expected_output(output),
            ConfigOptionKind::InterruptionReason(interruption_reason) => {
                config.interruption_reason(interruption_reason)
            },
        };
    }
    config.emit()
}

fn parse_test(path: &Path) -> io::Result<Test> {
    assert!(path.is_file());

    let content = read_to_string(path)?;

    let (content, comments) = CommentParser::default().parse(content);

    let options = comments
        .into_iter()
        .map(|com| com.into_config_option())
        .collect::<Vec<_>>();

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
        println!(
            "Running test `{}` with options {}",
            test.source.filename(),
            test.config
        );

        let interface = Interface::new(test.config.clone());

        let result = interface.compile_single_source(test.source);

        let (interruption_reason, sess) = match result {
            Err((interruption_reason, sess)) => (Some(interruption_reason), sess),
            Ok(sess) => (None, sess),
        };

        match (&interruption_reason, test.config.interruption_reason()) {
            (Some(interruption_reason), Some(expected_interruption_reason)) => {
                assert_eq!(interruption_reason, expected_interruption_reason);
            },
            _ => {},
        }

        let writer_data = sess.writer.data();

        println!("{}", writer_data);

        if let Some(expected_output) = test.config.expected_output() {
            let diff = TextDiff::configure()
                .newline_terminated(true)
                .diff_lines(expected_output.trim(), writer_data.trim());

            let do_differ = diff.ratio() < 1.0;

            if do_differ {
                println!(
                    "{} output differs from {}:",
                    "Actual".green(),
                    "expected".red()
                );
            }

            for diff in diff.iter_all_changes() {
                let color = match diff.tag() {
                    ChangeTag::Equal => Color::Black,
                    ChangeTag::Delete => Color::Red,
                    ChangeTag::Insert => Color::Green,
                };
                print!(
                    "{}",
                    format!(
                        "{}{}",
                        match diff.tag() {
                            ChangeTag::Equal => " ",
                            ChangeTag::Delete => "-",
                            ChangeTag::Insert => "+",
                        },
                        diff
                    )
                    .fg_color(color)
                );
            }

            if do_differ {
                panic!()
            }
        }
    })?;

    Ok(())
}
