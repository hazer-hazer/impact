use crate::span::span::Interner;

#[derive(Default)]
pub struct SessionGlobals {
    pub interner: Interner,
}

scoped_tls::scoped_thread_local!(static SESSION_GLOBALS: SessionGlobals);

pub fn with_session_globals<R, F>(f: F) -> R
where
    F: FnOnce(&SessionGlobals) -> R,
{
    SESSION_GLOBALS.with(f)
}

pub fn default_session_globals<R>(f: impl FnOnce() -> R) -> R {
    SESSION_GLOBALS.set(&SessionGlobals::default(), f)
}
