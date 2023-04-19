use super::{message::Message, MessageEmitter};
use crate::session::Session;

pub struct DebugEmitter {}

impl DebugEmitter {}

impl MessageEmitter for DebugEmitter {
    fn process_msg(&self, _: &Session, msg: &Message) {
        println!("{:?}", msg)
    }
}
