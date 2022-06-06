use crate::session::Session;

mod parser_stage;

struct PrettyPrinter<'a> {
    sess: &'a Session<'a>,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new(sess: &'a Session) -> Self {
        Self { sess: sess }
    }
}

trait PP {
    fn fmt<'a>(&self, sess: &'a Session) -> &'a str;
}
