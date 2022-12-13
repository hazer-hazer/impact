#![allow(dead_code)]
use std::fs::read_to_string;

use cli::color::Colorize;
use config::config::ConfigBuilder;
use config::config::{PPStages, StageName};
use interface::interface::{Interface, InterruptionReason};
use interface::writer::ConsoleWriter;
use message::message::MessageKind;
use session::Session;
use session::Source;

mod ast;
mod cli;
// mod codegen;
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
    let config = ConfigBuilder::new()
        .compilation_depth(StageName::Unset)
        .pp_stages(PPStages::All)
        .emit();

    let mut writer = Box::new(ConsoleWriter);
    let interface = Interface::new(config);

    let source_path = "examples/sample.imp";
    let source_file = read_to_string(source_path).unwrap();

    let source = Source::new(source_path.to_string(), source_file);

    let result = interface.compile_single_source(source, writer);

    if let Err(err) = result {
        match err {
            InterruptionReason::ConfiguredStop => {
                println!("Compilation stopped due to configured compilation depth");
            },
            InterruptionReason::Error(err) => {
                println!("{}", err.fg_color(MessageKind::Error.color()))
            },
        }
    }
}
