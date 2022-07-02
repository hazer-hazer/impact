use crate::{
    cli::color::Colorize,
    session::{LineInfo, Session, SpanSourceInfo},
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
            line:
                LineInfo {
                    num_indent,
                    num,
                    prev_line_index,
                    ..
                },
            pos_in_line,
            ..
        } = source.get_span_info(msg.span());

        // Header line `error: Message`
        println!(
            "{}",
            format!("{}: {}", msg.kind(), msg.text()).fg_color(msg.kind().color())
        );

        // Source-pointing line `[indent]--> file:line:col`
        println!(
            "{}--> {}:{}:{}",
            " ".repeat(num_indent),
            source.filename(),
            num,
            pos_in_line + 1
        );

        msg.labels()
            .iter()
            .for_each(|label| self.process_label(sess, label));

        println!()
    }
}

impl TermEmitter {
    fn print_line(
        &self,
        LineInfo {
            str,
            num_indent,
            num,
            ..
        }: LineInfo,
    ) {
        println!("{}{} | {}", " ".repeat(num_indent), num, str);
    }

    fn process_label(&self, sess: &Session, label: &Label) {
        let span = label.span();

        let source = sess.source_map.get_source(span.source());

        let SpanSourceInfo {
            line:
                line @ LineInfo {
                    num_len,
                    num_indent,
                    prev_line_index,
                    ..
                },
            pos_in_line,
        } = source.get_span_info(span);

        if let Some(index) = prev_line_index {
            self.print_line(source.get_line_info(index));
        }

        self.print_line(line);
        println!(
            "{}{}--- {}",
            // Indent of span pos in line + indent before number + number length + 3 for ` | `
            " ".repeat(pos_in_line as usize + num_indent + num_len + 3),
            "^".repeat(span.len() as usize),
            label.text()
        );
    }
}
