use crate::session::{Session, StageOutput, StageResult};

use self::message::Message;

pub mod debug_emitter;
pub mod message;
pub mod term_emitter;

pub trait MessageEmitter {
    fn emit<T>(&mut self, output: StageOutput<T>, stop_on_error: bool) -> StageResult<T> {
        let messages = output.messages;

        if cfg!(feature = "verbose_debug") {
            if messages.is_empty() {
                println!("Got no messages");
            } else {
                println!(
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
            self.process_msg(&output.sess, &msg);
        }

        if stop_on_error && self.got_error() {
            Err(String::from("Stop due to errors above"))
        } else {
            Ok((output.data, output.sess))
        }
    }

    fn got_error(&self) -> bool;

    fn error_appeared(&mut self);

    fn process_msg(&self, sess: &Session, msg: &Message);
}
