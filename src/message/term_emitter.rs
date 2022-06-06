use super::{MessageEmitter, message::Message};

pub struct TermEmitter {}

impl MessageEmitter for TermEmitter {
    fn process_msg(&self, msg: &Message) {
        
    }

    fn error_appeared(&mut self) {}
}
