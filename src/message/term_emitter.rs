use crate::{
    cli::color::Colorize,
    session::{LineInfo, Session, SpanSourceInfo},
};

use super::{
    message::{Label, Message, Solution, SolutionKind},
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
            lines, pos_in_line, ..
        } = source.get_span_info(msg.span());

        // Header line `error: Message
        println!(
            "{}",
            format!("{}: {}", msg.kind(), msg.text()).fg_color(msg.kind().color())
        );

        if sess.config().verbose_messages() {
            if let Some(origin) = msg.origin() {
                println!("{}", format!("Originated from {}", origin).yellow())
            }
        }

        let num_indent = lines.last().unwrap().num_indent;

        // TODO: Range for column too or just the start?
        // let num = if lines.len() == 1 {
        //     lines.first().unwrap().num.to_string()
        // } else {
        //     format!(
        //         "{}-{}",
        //         lines.first().unwrap().num,
        //         lines.last().unwrap().num
        //     )
        // };

        let num = lines.first().unwrap().num;

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

        if let Some(solution) = msg.solution() {
            self.process_solution(sess, solution);
        }

        println!();
    }
}

impl TermEmitter {
    fn print_line(
        &self,
        prefix: Option<&str>,
        LineInfo {
            str,
            num_indent,
            num,
            ..
        }: &LineInfo,
    ) {
        println!(
            "{}{} |{} {}",
            " ".repeat(*num_indent),
            num,
            prefix.unwrap_or(""),
            str
        );
    }

    fn process_label(&self, sess: &Session, label: &Label) {
        let span = label.span();

        let source = sess.source_map.get_source(span.source());

        let SpanSourceInfo { lines, pos_in_line } = source.get_span_info(span);

        // Always print previous line before span for convenience
        if let Some(index) = lines.first().unwrap().prev_line_index {
            self.print_line(None, &source.get_line_info(index));
        }

        if lines.len() == 1 {
            let line @ LineInfo {
                num_len,
                num_indent,
                ..
            } = lines.first().unwrap();

            self.print_line(None, line);
            println!(
                "{}{}--- {}",
                // Indent of span pos in line + indent before number + number length + 3 for ` | `
                " ".repeat(pos_in_line as usize + num_indent + num_len + 3),
                "^".repeat(span.len() as usize),
                label.text()
            );
        } else {
            let LineInfo {
                num_len,
                num_indent,
                pos,
                ..
            } = lines.first().unwrap();

            if span.lo() != *pos {
                println!(
                    "...{}v-- from here",
                    " ".repeat(pos_in_line as usize + num_indent + num_len + 1)
                );
            }

            lines.iter().for_each(|line| {
                self.print_line(Some(" >"), line);
            });

            println!(
                "{}\\--- {}",
                " ".repeat(num_indent + num_len + 3),
                label.text()
            );
        }
    }

    fn process_solution(&self, sess: &Session, solution: &Solution) {
        match solution.kind() {
            SolutionKind::Rename { kind, name, to } => self.process_label(
                sess,
                &Label::new(
                    name.span(),
                    format!(
                        "Solution: Rename {} `{}` {}",
                        kind,
                        name.sym(),
                        to.as_ref()
                            .map_or("".to_string(), |to| format!("to {}", to))
                    ),
                ),
            ),
        }
    }
}
