extern crate string_interner;

use message::debug_emitter::DebugEmitter;
use parser::lexer::Lexer;
use pp::PP;
use session::{Stage, Session};

mod dt;
mod parser;
mod message;
mod session;
mod span;
mod pp;

fn main() {
    let sess = Session::default();

    let mut emitter = DebugEmitter::default();

    let source = "
    if a
        print kek
    
    ";
    let (tokens, sess) = Lexer::new(source, sess).run_and_unwrap(&mut emitter);

    // println!("{}", source);
    println!("{}", tokens.ppfmt(&sess))
}
