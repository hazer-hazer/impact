extern crate string_interner;

use parser::{lexer::Lexer, parser::Parser};
use pp::PP;
use session::{Session, Stage};

use crate::{ast::visitor::Visitor, pp::AstLikePP, message::term_emitter::TermEmitter};

mod ast;
mod dt;
mod message;
mod parser;
mod pp;
mod session;
mod span;
mod typeck;
mod hir;

fn main() {
    let sess = Session::default();

    let source = "let a = 123";

    let (tokens, sess) = Lexer::new(source, sess).run_and_unwrap();

    let term_emitter = TermEmitter::new(&sess);
    term_emitter.print_source();

    // println!("{}", tokens.ppfmt(&sess));

    let (ast, sess) = Parser::new(sess, tokens).run_and_unwrap();

    let mut pp = AstLikePP::new(&sess);
    println!("{}", pp.visit_ast(&ast));
}
