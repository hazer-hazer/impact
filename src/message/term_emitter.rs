use crate::session::Session;

use super::{MessageEmitter, message::Message};

pub struct TermEmitter<'a> {
    sess: Session<'a>,
    got_error: bool,
}

impl<'a> MessageEmitter<'a> for TermEmitter<'a> {
    fn error_appeared(&mut self) {
        self.got_error = true;
    }

    fn process_msg(&self, msg: &Message) {
        
    }

    fn set_sess(&mut self, sess: Session<'a>) {
        self.sess = sess;
    }
}
