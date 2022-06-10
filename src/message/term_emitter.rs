use crate::session::Session;

use super::{message::Message, MessageEmitter};

pub struct TermEmitter<'a> {
    sess: &'a Session,
    got_error: bool,
}

impl<'a> TermEmitter<'a> {
    pub fn new(sess: &'a Session) -> Self {
        Self {
            sess,
            got_error: false,
        }
    }
}

impl<'a> MessageEmitter for TermEmitter<'a> {
    fn error_appeared(&mut self) {
        self.got_error = true;
    }

    fn process_msg(&self, msg: &Message) {
        let span = msg.span();
        let (line, line_pos) = self.sess.source_lines().find_line(span);
        assert!(line_pos <= span.pos);

        println!("{}", line);
        print!(
            "{}{} --- {}\n",
            " ".repeat((span.pos - line_pos) as usize),
            "^".repeat(span.len as usize),
            msg.text()
        );
    }
}
