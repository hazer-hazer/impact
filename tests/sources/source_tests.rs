use std::fs::read_to_string;

use impact::cli::color::Colorize;
use impact::config::config::{Config, PPStages, StageName};
use impact::interface::interface::Interface;
use impact::message::message::MessageKind;
use impact::session::Source;

mod impact::ast;
mod impact::cli;
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

#[test]
fn test_sources() {
    assert!(1)
}
