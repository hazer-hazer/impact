use crate::session::Session;

use super::{MessageEmitter, message::{Message}};

#[derive(Default)]
struct DebugEmitter {
    sess: Session,
}

impl MessageEmitter for DebugEmitter {
    fn process_msg(&self, msg: &Message) {
        
    }

    fn set_sess(&mut self, sess: Session) {
        self.sess = sess;
    }

    fn error_appeared(&mut self) {}
}
