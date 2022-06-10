use std::fmt::Debug;

use crate::session::Session;

use super::{message::Message, MessageEmitter};

pub struct DebugEmitter {}

impl DebugEmitter {}

impl MessageEmitter for DebugEmitter {
    fn process_msg(&self, msg: &Message) {
        println!("{:?}", msg);
    }

    fn error_appeared(&mut self) {}
}
