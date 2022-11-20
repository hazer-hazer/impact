use std::fs::read_to_string;

use cli::color::Colorize;
use config::config::{Config, PPStages, StageName};
use interface::interface::Interface;
use message::message::MessageKind;
use session::Source;

mod ast;
mod cli;
mod codegen;
mod config;
mod hir;
mod interface;
mod lower;
mod message;
mod parser;
mod pp;
mod resolve;
mod session;
mod span;
mod typeck;

fn main() {
    let config = Config {
        compilation_depth: StageName::Unset,
        pp_stages: PPStages::All,
    };
    let interface = Interface::new(config);

    let source_path = "examples/sample.imp";
    let source_file = read_to_string(source_path).unwrap();

    let source = Source::new(source_path.to_string(), source_file);

    let result = interface.compile_single_source(source);

    if let Err(err) = result {
        println!("{}", err.fg_color(MessageKind::Error.color()))
    }
}
