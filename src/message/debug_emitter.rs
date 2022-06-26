use crate::session::Session;

use super::{message::Message, MessageEmitter};

pub struct DebugEmitter {}

impl DebugEmitter {}

impl MessageEmitter for DebugEmitter {
    fn process_msg(&self, sess: &Session, msg: &Message) {
        println!("{:?}", msg)
    }

    fn got_error(&self) -> bool {
        // TODO: ?
        false
    }

    fn error_appeared(&mut self) {}
}
