use crate::session::Session;

use self::message::Message;

pub mod debug_emitter;
pub mod message;
pub mod term_emitter;

pub trait MessageEmitter {
    fn emit(&mut self, sess: Session, messages: Vec<Message>) -> Session {
        for msg in messages.iter() {
            if msg.is(message::MessageKind::Error) {
                self.error_appeared();
            }
            self.process_msg(&msg);
        }

        sess
    }

    fn error_appeared(&mut self);

    fn process_msg(&self, msg: &Message);
}
