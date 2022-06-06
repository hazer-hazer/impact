use crate::session::Session;

use super::{MessageEmitter, message::Message};

pub struct TermEmitter {
    sess: Session,
    got_error: bool,
}

impl MessageEmitter for TermEmitter {
    fn error_appeared(&mut self) {
        self.got_error = true;
    }

    fn process_msg(&self, msg: &Message) {
        
    }

    fn set_sess(&mut self, sess: Session) {
        self.sess = sess;
    }
}
