use config::config::{Config, PPStages, StageName};
use interface::interface::Interface;

mod ast;
mod cli;
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
// mod typeck;

fn main() {
    let config = Config {
        compilation_depth: StageName::Unset,
        pp_stages: PPStages::All,
    };
    let interface = Interface::new(config);

    let result = interface.compile_single_source("let a = 123 in a");

    if let Err(err) = result {
        println!("{}", err)
    }
}
