use crate::{
    message::message::{Message, MessageHolder},
    span::span::{Interner, Symbol},   
};

/**
 * Session is a compilation context passed through all stages of compilation.
 */

#[derive(Default)]
pub struct Session<'a> {
    interner: Interner<'a>,
}

impl<'a> Session<'a> {
    pub fn get_str(&self, sym: Symbol) -> &str {
        self.interner.get_str(sym)
    }

    pub fn intern(&self, string: &'a str) -> Symbol {
        self.interner.intern(string)
    }
}

pub struct Result<'a, T> {
    sess: Session<'a>,
    data: T,
    messages: Vec<Message>,
}

impl<'a, T> Result<'a, T> {
    pub fn new(sess: Session<'a>, data: T, message_holder: MessageHolder) -> Self {
        Self {
            sess,
            data,
            messages: message_holder.extract(),
        }
    }
}

pub trait Stage<T> {
    fn run<'a>(self, sess: Session<'a>) -> Result<'a, T>;
}
