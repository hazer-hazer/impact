use self::message::Message;
use crate::{
    cli::verbose,
    session::{Session, StageOutput, StageResult, WithSession, InterruptionReason},
};

pub mod debug_emitter;
pub mod human_lang;
pub mod message;
pub mod term_emitter;

pub trait MessageEmitter {
    fn emit<T, Ctx>(
        &mut self,
        output: StageOutput<T, Ctx>,
        stop_on_error: bool,
    ) -> StageResult<T, Ctx>
    where
        Ctx: WithSession,
    {
        let messages = output.messages;
        let sess = output.ctx.sess();

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

        for msg in messages.iter() {
            if msg.is(message::MessageKind::Error) {
                self.error_appeared();
            }
            self.process_msg(&sess, &msg);
        }

        if stop_on_error && self.got_error() {
            Err((InterruptionReason::ErrorMessage, output.ctx))
        } else {
            Ok((output.data, output.ctx))
        }
    }

    fn got_error(&self) -> bool;

    fn error_appeared(&mut self);

    fn process_msg(&self, sess: &Session, msg: &Message);
}
