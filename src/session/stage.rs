use super::{Session, SessionHolder};
use crate::message::{message::MessageStorage, MessageEmitter};

/// Stage is a common trait implemented by all compilation stages and their
/// substeps.
/// Each stage returns result, containing data it produces, context it work with
/// and messages it produces.
///
/// 1. Context is a result of stage because of moves. For example, Session is
/// moved almost throughout all stages and we need to get it back from each
/// stage to run next.
///
/// 2. Why move stage back on Err result? - Because some stages are recoverable
/// and next ones can be ran no matters has error occurred on previous one or
/// not.
///
/// 3. Why store messages on Ok result? - Because some messages are infallible,
/// e.g. warnings which we do output but not continue compilation.
///
/// Stage is trying to be written in such a way that these obligations are met:
/// 1. Have no access to produced data and context until messages are checked
/// for errors.
///
/// 2. It must not be possible to create Ok result with error messages inside.

pub trait Stage<T, Ctx = Session>
where
    Self: Sized,
{
    fn run(self) -> StageResult<T, Ctx>;

    fn run_and_emit(
        self,
        emitter: &mut impl MessageEmitter,
    ) -> Result<(T, Ctx), InterruptionErr<Ctx>>
    where
        Ctx: SessionHolder,
    {
        self.run().emit(emitter)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterruptionReason {
    ConfiguredStop,
    ErrorMessage,
}

impl InterruptionReason {
    pub fn from_str(str: &str) -> Self {
        match str {
            "configured" => Self::ConfiguredStop,
            "error" => Self::ErrorMessage,
            _ => panic!("Invalid interruption reason name"),
        }
    }
}

impl std::fmt::Display for InterruptionReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InterruptionReason::ConfiguredStop => "configured",
                InterruptionReason::ErrorMessage => "error",
            }
        )
    }
}

pub type InterruptionErr<Ctx> = (InterruptionReason, Ctx);
pub type InterruptResult<Ctx = Session> = Result<Ctx, InterruptionErr<Ctx>>;

pub struct StageOk<T, Ctx = Session> {
    ctx: Ctx,
    data: T,
    msg: MessageStorage,
}

impl<T, Ctx> StageOk<T, Ctx> {
    // Some ways to unwrap StageOk data without emitting message but preserving them

    /// Move messages to some base MessageStorage to preserve them.
    /// Now we can get our data.
    pub fn merged(self, base_msg: &mut MessageStorage) -> (T, Ctx) {
        base_msg.merge(self.msg);
        (self.data, self.ctx)
    }

    pub fn map_ctx<F, U>(self, f: F) -> StageOk<T, U>
    where
        F: FnOnce(Ctx) -> U,
    {
        StageOk {
            ctx: f(self.ctx),
            data: self.data,
            msg: self.msg,
        }
    }
}

pub struct StageErr<Ctx = Session> {
    reason: InterruptionReason,
    ctx: Ctx,
    msg: MessageStorage,
}

impl<Ctx> StageErr<Ctx> {
    pub fn ctx(&self) -> &Ctx {
        &self.ctx
    }

    pub fn msg(&self) -> &MessageStorage {
        &self.msg
    }

    pub fn map_ctx<F, U>(self, f: F) -> StageErr<U>
    where
        F: FnOnce(Ctx) -> U,
    {
        StageErr {
            ctx: f(self.ctx),
            reason: self.reason,
            msg: self.msg,
        }
    }
}

pub type StageResult<T, Ctx = Session> = Result<StageOk<T, Ctx>, StageErr<Ctx>>;

pub fn stage_result<T, Ctx>(ctx: Ctx, data: T, msg: MessageStorage) -> StageResult<T, Ctx> {
    if msg.check_for_errors() {
        Err(StageErr {
            reason: InterruptionReason::ErrorMessage,
            ctx,
            msg,
        })
    } else {
        Ok(StageOk { ctx, data, msg })
    }
}

pub struct RecoveredStageResult<T, Ctx = Session> {
    ctx: Ctx,
    data: T,
    msg: MessageStorage,
}

impl<T, Ctx> RecoveredStageResult<T, Ctx> {
    pub fn ctx(&self) -> &Ctx {
        &self.ctx
    }

    pub fn ctx_mut(&mut self) -> &mut Ctx {
        &mut self.ctx
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn emit(
        mut self,
        emitter: &mut impl MessageEmitter,
    ) -> Result<(T, Ctx), InterruptionErr<Ctx>>
    where
        Ctx: SessionHolder,
    {
        let (error_occurred, text) = emitter.emit(self.msg, &mut self.ctx);
        if error_occurred.into() {
            Err((InterruptionReason::ErrorMessage, self.ctx))
        } else {
            Ok((self.data, self.ctx))
        }
    }
}

pub trait StageResultImpl<T, Ctx = Session> {
    fn ctx(&self) -> &Ctx;

    fn map_ctx<U>(self, f: impl FnOnce(Ctx) -> U) -> StageResult<T, U>;
    fn unit_ctx(self) -> StageResult<T, ()>;

    fn recover(self) -> Result<RecoveredStageResult<T, Ctx>, InterruptionErr<Ctx>>;

    fn emit(self, emitter: &mut impl MessageEmitter) -> Result<(T, Ctx), InterruptionErr<Ctx>>
    where
        Ctx: SessionHolder;
}

impl<T, Ctx> StageResultImpl<T, Ctx> for StageResult<T, Ctx> {
    fn ctx(&self) -> &Ctx {
        match self {
            Ok(ok) => &ok.ctx,
            Err(err) => &err.ctx,
        }
    }

    fn map_ctx<U>(self, f: impl FnOnce(Ctx) -> U) -> StageResult<T, U> {
        match self {
            Ok(ok) => Ok(ok.map_ctx(f)),
            Err(err) => Err(err.map_ctx(f)),
        }
    }

    fn unit_ctx(self) -> StageResult<T, ()> {
        self.map_ctx(|_| ())
    }

    fn recover(self) -> Result<RecoveredStageResult<T, Ctx>, InterruptionErr<Ctx>> {
        match self {
            Ok(ok) => Ok(RecoveredStageResult {
                ctx: ok.ctx,
                data: ok.data,
                msg: ok.msg,
            }),
            Err(err) => Err((InterruptionReason::ErrorMessage, err.ctx)),
        }
    }

    fn emit(self, emitter: &mut impl MessageEmitter) -> Result<(T, Ctx), InterruptionErr<Ctx>>
    where
        Ctx: SessionHolder,
    {
        match self {
            Ok(mut ok) => {
                let (error_occurred, text) = emitter.emit(ok.msg, &mut ok.ctx);
                if error_occurred.into() {
                    Err((InterruptionReason::ErrorMessage, ok.ctx))
                } else {
                    Ok((ok.data, ok.ctx))
                }
            },
            Err(mut err) => {
                let (_its_already_an_error, text) = emitter.emit(err.msg, &mut err.ctx);
                err.ctx.sess_mut().writer.write(text);
                Err((InterruptionReason::ErrorMessage, err.ctx))
            },
        }
    }
}
