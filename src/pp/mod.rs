use crate::session::Session;

mod parser_stage;

struct PrettyPrinter<'a> {
    sess: &'a Session,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new(sess: &'a Session) -> Self {
        Self { sess: sess }
    }
}

pub trait PP<'a> {
    fn ppfmt(&self, sess: &'a Session) -> String;
}
