use clap::Parser;
use clap::{arg, command};

use crate::config::config::PPStages;

#[derive(Parser, Debug)]
#[command(
    author = "hazer-hazer",
    version = "0.1",
    about = "Impact programming language compiler"
)]
struct Args {
    /// Pretty-print compilation stages
    #[arg(long)]
    pp_stages: Option<PPStages>,
}

pub struct CLI {}

impl CLI {}
