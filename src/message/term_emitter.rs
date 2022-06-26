use crate::session::Session;

use super::{message::Message, MessageEmitter};

pub struct TermEmitter {
    got_error: bool,
}

impl TermEmitter {
    pub fn new() -> Self {
        Self { got_error: false }
    }

    // TODO: Rewrite
    // pub fn print_source(&self) {
    //     println!("=== SOURCE ===");
    //     let source_lines = self.sess.source_lines().lines();
    //     let last_line_num = source_lines.len() + 1;

    //     for (i, line) in source_lines.iter().enumerate() {
    //         let line_num = i + 1;
    //         println!(
    //             "{}{} | {}",
    //             " ".repeat(line_num.to_string().len() - last_line_num.to_string().len()),
    //             line_num,
    //             line
    //         );
    //     }

    //     println!("=== SOURCE END ===\n");
    // }
}

impl MessageEmitter for TermEmitter {
    fn error_appeared(&mut self) {
        self.got_error = true;
    }

    fn got_error(&self) -> bool {
        self.got_error
    }

    fn process_msg(&self, sess: &Session, msg: &Message) {
        let span = msg.span();

        let source = sess.source_map.get_source(span.source());
        let lines_count = source.lines_count();

        let (line, line_pos, line_num) = if span.is_error() {
            ("[The place my stupid mistakes live]", 0, 0)
        } else {
            let (line, line_pos, line_num) = source.find_line(span);
            assert!(line_pos <= span.lo());

            (line, line_pos, line_num)
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
