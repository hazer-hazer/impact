use std::default;

use crate::session::Session;

use super::{MessageEmitter, message::{self, Message}};

#[derive(Default)]
struct DebugEmitter {
    got_error: bool,
}

impl MessageEmitter for DebugEmitter {
    fn error_appeared(&mut self) {
        self.got_error = true;
    }

    fn process_msg(&self, msg: &Message) {
        
    }
}
