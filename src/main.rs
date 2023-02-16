#![allow(dead_code)]
#![deny(unconditional_recursion)]

use std::fs::read_to_string;

use cli::color::Colorize;
use config::config::ConfigBuilder;
use config::config::{PPStages, StageName};
use interface::interface::{Interface, InterruptionReason};

use session::Source;

mod ast;
mod cli;
mod codegen;
mod config;
mod dt;
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
        .verbose_messages(true)
        .parser_debug(false)
        .pp_ast_ids(false)
        .emit();

    let interface = Interface::new(config);

    let source_path = "examples/sample.imp";
    let source_file = read_to_string(source_path).unwrap();

    let source = Source::new(source_path.to_string(), source_file);

    let result = interface.compile_single_source(source);

    let (interruption_reason, sess) = match result {
        Err((interruption_reason, sess)) => (Some(interruption_reason), sess),
        Ok(sess) => (None, sess),
    };

    println!("{}", sess.writer.data());

    if let Some(interruption_reason) = interruption_reason {
        match interruption_reason {
            InterruptionReason::ConfiguredStop => {
                println!("Compilation stopped due to configured compilation depth");
            },
            InterruptionReason::ErrorMessage => {
                println!("{}", "Stop due to errors above".red())
            },
        }
    }
}
