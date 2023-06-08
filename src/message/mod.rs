use self::message::{Message, MessageStorage};
use crate::{
    cli::{color::Colorize, verbose},
    dt::bool_enum::bool_enum,
    session::{Session, SessionHolderMut},
};

pub mod debug_emitter;
pub mod human_lang;
pub mod message;
pub mod term_emitter;

bool_enum!(ErrMessageOccurred);

pub trait MessageEmitter {
    fn emit<Ctx>(&mut self, msg: MessageStorage, ctx: &mut Ctx) -> ErrMessageOccurred
    where
        Ctx: SessionHolderMut,
    {
        let sess = ctx.sess_mut();
        let messages = msg.extract();

        if cfg!(feature = "verbose_debug") {
            if messages.is_empty() {
                verbose!("{}", "Got no messages".yellow());
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

            self.process_msg(sess, &msg);
        }

        error_appeared.into()
    }

    fn process_msg(&mut self, sess: &mut Session, msg: &Message);
}
