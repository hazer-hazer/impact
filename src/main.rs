#![deny(unconditional_recursion)]
#![allow(dead_code)]

use std::fs::read_to_string;

use cli::color::Colorize;
use interface::interface::{Interface, InterruptionReason};

use crate::{
    cli::{cli::CLI, verbose},
    span::source::Source,
};

mod ast;
mod cli;
mod codegen;
mod config;
mod dt;
mod hir;
mod interface;
mod lower;
mod message;
mod mir;
mod parser;
mod pp;
mod resolve;
mod session;
mod span;
mod typeck;
mod utils;

fn main() {
    let config = CLI::new().config().clone();
    verbose!("Config: {}", config);

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
