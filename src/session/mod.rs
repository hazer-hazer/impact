use string_interner::{StringInterner};

use crate::{
    message::message::{Message, MessageHolder},
    span::span::Symbol,
};

/**
 * Session is a compilation context passed through all stages of compilation.
 */

#[derive(Default)]
pub struct Session {
    interner: StringInterner,
}

impl Session {
    pub fn intern<T>(&mut self, string: T) -> Symbol where T: AsRef<str> {
        Symbol::new(self.interner.get_or_intern(string))
    }

    pub fn get_str(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym.as_inner()).expect(format!("Failed to resolve symbol {sym:?}").as_str())
    }
}

pub struct StageResult<T> {
    sess: Session,
    data: T,
    messages: Vec<Message>,
}

impl<T> StageResult<T> {
    pub fn new(sess: Session, data: T, message_holder: MessageHolder) -> Self {
        Self {
            sess,
            data,
            messages: message_holder.extract(),
        }
    }
}

pub trait Stage<T> {
    fn run(self, sess: Session) -> StageResult<T>;
}
