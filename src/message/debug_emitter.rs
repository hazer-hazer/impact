use clap::error;

use super::{message::Message, MessageEmitter};
use crate::{
    interface::writer::{outln, Writer},
    session::Session,
};

pub struct DebugEmitter {
    writer: Writer,
}

impl DebugEmitter {}

impl MessageEmitter for DebugEmitter {
    fn emit<Ctx>(
        mut self,
        msg: super::message::MessageStorage,
        ctx: &Ctx,
    ) -> (super::ErrMessageOccurred, String)
    where
        Ctx: crate::session::SessionHolder,
    {
        let mut error_occurred = false;
        let text = msg
            .extract()
            .iter()
            .map(|msg| {
                if msg.is(crate::message::message::MessageKind::Error) {
                    error_occurred = true
                };
                format!("{:?}", msg)
            })
            .collect();

        (error_occurred.into(), text)
    }
}
