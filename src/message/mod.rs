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
    fn emit<Ctx>(self, msg: MessageStorage, ctx: &Ctx) -> (ErrMessageOccurred, String)
    where
        Ctx: SessionHolder;
}
