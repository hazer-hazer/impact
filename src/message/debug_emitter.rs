use crate::session::Session;

use super::{MessageEmitter, message::{Message}};

#[derive(Default)]
struct DebugEmitter<'a> {
    sess: Session<'a>,
}

impl<'a> MessageEmitter<'a> for DebugEmitter<'a> {
    fn process_msg(&self, msg: &Message) {
        
    }

    fn set_sess(&mut self, sess: Session<'a>) {
        self.sess = sess;
    }

    fn error_appeared(&mut self) {}
}
