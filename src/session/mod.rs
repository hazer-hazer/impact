use crate::message::message::{MessageHolder, Message};

/**
 * Session is a compilation context passed through all stages of compilation.
 */

#[derive(Default, Debug)]
pub struct Session {
    interner: u32,
}

pub struct Result<T> {
    sess: Session,
    data: T,
    messages: Vec<Message>,
}

impl<T> Result<T> {
    pub fn new(sess: Session, data: T, message_holder: MessageHolder) -> Self {
        Self {
            sess,
            data,
            messages: message_holder.extract(),
        }
    }
}

pub trait Stage<T> {
    fn run(self, sess: Session) -> Result<T>;
}
