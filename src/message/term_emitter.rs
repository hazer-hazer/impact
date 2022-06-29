use crate::{
    cli::color::Colorize,
    session::{Session, SpanSourceInfo},
};

use super::{
    message::{Label, Message},
    MessageEmitter,
};

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
        let source = sess.source_map.get_source(msg.span().source());

        let SpanSourceInfo {
            line_num_indent,
            line_num,
            pos_in_line,
            ..
        } = source.get_line_info(sess, msg.span());

        println!(
            "{}\n{}--> {}:{}:{}",
            format!("{}: {}", msg.kind(), msg.text()).fg_color(msg.kind().color()),
            " ".repeat(line_num_indent),
            source.filename(),
            line_num,
            pos_in_line + 1
        );

        msg.labels()
            .iter()
            .for_each(|label| self.process_label(sess, label));

        println!()
    }
}

impl TermEmitter {
    fn process_label(&self, sess: &Session, label: &Label) {
        let span = label.span();

        let source = sess.source_map.get_source(span.source());

        let SpanSourceInfo {
            line,
            line_num,
            line_num_len,
            line_num_indent,
            pos_in_line,
            ..
        } = source.get_line_info(sess, span);

        println!("{}{} | {}", " ".repeat(line_num_indent), line_num, line);
        println!(
            "{}{}--- {}",
            // Indent of span pos in line + indent before number + number length + 3 for ` | `
            " ".repeat(pos_in_line as usize + line_num_indent + line_num_len + 3),
            "^".repeat(span.len() as usize),
            label.text()
        );
    }
}
