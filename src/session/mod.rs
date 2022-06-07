use std::fmt::Debug;

use string_interner::{StringInterner};

use crate::{
    message::{message::{Message, MessageHolder}, MessageEmitter},
    span::span::Symbol,
};

/**
 * Session is a compilation context passed through all stages of compilation.
 */

#[derive(Default)]
pub struct Session {
    interner: StringInterner,
}

pub struct WithSession<'a, T> {
    sess: &'a Session,
    val: &'a T,
}

impl Session {
    pub fn with_sess<'a, T>(&'a self, val: &'a T) -> WithSession<'a, T> {
        WithSession { sess: self, val: val }
    }

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

pub type OkStageResult<T> = (T, Session);

impl<T> StageResult<T> {
    pub fn new(sess: Session, data: T, message_holder: MessageHolder) -> Self {
        Self {
            sess,
            data,
            messages: message_holder.extract(),
        }
    }

    pub fn unwrap(self, emitter: &mut impl MessageEmitter) -> OkStageResult<T> {
        emitter.emit(&self.sess, self.messages);

        (self.data, self.sess)
    }
}

pub trait Stage<T> {
    fn run(self) -> StageResult<T>;

    fn run_and_unwrap(self, emitter: &mut impl MessageEmitter) -> OkStageResult<T>;
}
