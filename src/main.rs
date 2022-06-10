extern crate string_interner;

use message::debug_emitter::DebugEmitter;
use parser::{lexer::Lexer, parser::Parser};
use pp::PP;
use session::{Stage, Session};

use crate::{pp::ast::AstPP, parser::ast::visitor::Visitor};

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
    let a = 123
    ";
    
    let (tokens, sess) = Lexer::new(source, sess).run_and_unwrap(&mut emitter);

    println!("{:?}", sess.source_lines());
    println!("{}", tokens.ppfmt(&sess));

    let (ast, sess) = Parser::new(sess, tokens).run_and_unwrap(&mut emitter);

    let mut pp = AstPP::new(&sess);
    pp.visit_ast(&ast);
}
