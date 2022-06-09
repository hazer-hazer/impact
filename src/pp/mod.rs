use crate::session::Session;

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

impl<'a, T> PP<'a> for Box<T>
where
    T: PP<'a>,
{
    fn ppfmt(&self, sess: &'a Session) -> String {
        format!("{}", self.as_ref().ppfmt(sess))
    }
}
