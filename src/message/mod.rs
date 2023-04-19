use self::message::{Message, MessageStorage};
use crate::{
    cli::verbose,
    dt::bool_enum::bool_enum,
    session::{Session, SessionHolder},
};

pub mod debug_emitter;
pub mod human_lang;
pub mod message;
pub mod term_emitter;

bool_enum!(ErrMessageOccurred);

pub trait MessageEmitter {
    fn emit<Ctx>(&mut self, msg: MessageStorage, ctx: &Ctx) -> ErrMessageOccurred
    where
        Ctx: SessionHolder,
    {
        let sess = ctx.sess();
        let messages = msg.extract();

        if cfg!(feature = "verbose_debug") {
            if messages.is_empty() {
                verbose!("Got no messages");
            } else {
                verbose!(
                    "Printing messages as are\n{}",
                    messages
                        .iter()
                        .map(|m| format!("{m:?}"))
                        .collect::<Vec<_>>()
                        .join("\n")
                );
            }
        }

        let mut error_appeared = false;

        for msg in messages.iter() {
            if msg.is(message::MessageKind::Error) {
                error_appeared = true;
            }
            self.process_msg(&sess, &msg);
        }

        error_appeared.into()
    }

    fn process_msg(&self, sess: &Session, msg: &Message);
}
