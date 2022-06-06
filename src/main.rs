use parser::lexer::Lexer;

use crate::parser::token::{Token, TokenKind};
use crate::span::span::{Span, Symbol};

mod dt;
mod parser;
mod message;
mod session;
mod span;

fn main() {
    let tokens = Lexer::new("a b").lex();

    let tok;
    let ident = String::from("c");
    {
        let sym = Symbol::intern(ident.as_str());
        println!("ident {:?} and sym {:?}", ident, sym);
        {
            tok = Token {
                span: Span { pos: 0, len: 1 },
                kind: TokenKind::Ident(sym),
            };
        }
    }

    println!("c is {:?}", tok);
    // println!("ident is {:?}", ident);

    println!("{:?}", tokens);
}
