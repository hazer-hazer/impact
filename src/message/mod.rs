use crate::session::Session;

use self::message::Message;

pub mod debug_emitter;
pub mod message;
pub mod term_emitter;

pub trait MessageEmitter {
    fn emit<'a>(&mut self, messages: Vec<Message>) {
        for msg in messages.iter() {
            if msg.is(message::MessageKind::Error) {
                self.error_appeared();
            }
            self.process_msg(&msg);
        }
    }

    fn error_appeared(&mut self);

    fn process_msg(&self, msg: &Message);
}
