use clap::error;

use super::{message::Message, MessageEmitter};
use crate::{
    interface::writer::{outln, Writer},
    session::Session,
};

pub struct DebugEmitter {}

impl DebugEmitter {
    pub fn new() -> Self {
        Self {}
    }
}

impl MessageEmitter for DebugEmitter {
    fn process_msg(&mut self, sess: &mut Session, msg: &Message) {
        outln!(dbg, sess.writer, "{:?}", msg);
    }
}
