extern crate string_interner;

use parser::lexer::Lexer;
use session::{Stage, Session};

mod dt;
mod parser;
mod message;
mod session;
mod span;
mod pp;

fn main() {
    let sess = Session::default();

    let source = "a b";
    // let result = Lexer::new(source).run(sess);
}
