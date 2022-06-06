use crate::session::Session;

use self::message::Message;

pub mod debug_emitter;
pub mod message;
pub mod term_emitter;

pub trait MessageEmitter<'a> {
    fn emit(&mut self, sess: Session<'a>, messages: Vec<Message>) -> Session<'a> {
        for msg in messages.iter() {
            if msg.is(message::MessageKind::Error) {
                self.error_appeared();
            }
            self.process_msg(&msg);
        }

        sess
    }

    fn set_sess(&mut self, sess: Session<'a>);

    fn error_appeared(&mut self);

    fn process_msg(&self, msg: &Message);
}
