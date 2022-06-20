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

    pub fn print_source(&self) {
        println!("=== SOURCE ===");
        let source_lines = self.sess.source_lines().get_lines();
        let last_line_num = source_lines.len() + 1;

        for (i, line) in source_lines.iter().enumerate() {
            let line_num = i + 1;
            println!(
                "{}{} | {}",
                " ".repeat(line_num.to_string().len() - last_line_num.to_string().len()),
                line_num,
                line
            );
        }

        println!("=== SOURCE END ===\n");
    }
}

impl<'a> MessageEmitter for TermEmitter<'a> {
    fn error_appeared(&mut self) {
        self.got_error = true;
    }

    fn process_msg(&self, msg: &Message) {
        let span = msg.span();

        let source_lines = self.sess.source_lines();
        let lines_count = source_lines.get_lines().len();

        let (line, line_pos, line_num) = if span.is_error() {
            ("[The place my stupid mistakes live]", 0, 0)
        } else {
            let (line, line_pos, line_num) = source_lines.find_line(span);
            assert!(line_pos <= span.lo());

            (line.as_str(), line_pos, line_num)
        };

        let line_num_len = line_num.to_string().len();
        let line_num_indent = line_num_len - (lines_count + 1).to_string().len();

        println!("{}{} | {}", " ".repeat(line_num_indent), line_num, line);
        print!(
            "{}{}--- {}\n",
            // Indent of span pos in line + indent before number + number length + 3 for ` | `
            " ".repeat((span.lo() - line_pos) as usize + line_num_indent + line_num_len + 3),
            "^".repeat(span.len() as usize),
            msg.text()
        );
    }
}
