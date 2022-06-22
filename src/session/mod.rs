use crate::{
    ast::{AstMetadata, NodeId},
    message::{
        message::{Message, MessageStorage},
        term_emitter::TermEmitter,
        MessageEmitter,
    },
    span::span::{Span, SpanPos},
};

/**
 * Session is a compilation context passed through all stages of compilation.
 */

#[derive(Default)]
pub struct SourceLines {
    lines: Vec<String>,
    positions: Vec<SpanPos>,
    source_size: usize,
}

impl SourceLines {
    pub fn new(source_size: usize) -> Self {
        Self {
            source_size,
            ..Default::default()
        }
    }

    // FIXME: This is a design, don't use setters,
    //  just create session with source
    //  lines using constructor (requires some work with Default's)
    pub fn set_source_size(&mut self, source_size: usize) {
        self.source_size = source_size;
    }

    pub fn add_line(&mut self, line: String, pos: SpanPos) {
        self.lines.push(line);
        self.positions.push(pos);
    }

    /// get (line string, line position, line number)
    pub fn find_line(&self, span: Span) -> (&String, SpanPos, usize) {
        if span.is_error() {
            panic!()
        }

        for i in 0..self.lines.len() {
            let line_pos = self.positions[i];
            let next_line_pos = *self
                .positions
                .get(i + 1)
                .unwrap_or(&(self.source_size as u32));

            // We encountered line further than span
            if span.lo() < line_pos {
                break;
            }

            if span.lo() >= line_pos && span.lo() < next_line_pos {
                return (&self.lines[i], line_pos, i + 1);
            }
        }

        panic!("No source line found for span {}", span);
    }

    pub fn get_lines(&self) -> &Vec<String> {
        &self.lines
    }

    pub fn get_positions(&self) -> &Vec<SpanPos> {
        &self.positions
    }
}

#[derive(Default)]
pub struct Session {
    source_lines: SourceLines,
    ast_metadata: AstMetadata,
}

impl Session {
    pub fn with_sess<'a, T>(&'a self, val: &'a T) -> WithSession<'a, T> {
        WithSession {
            sess: self,
            val: val,
        }
    }

    pub fn source_lines(&self) -> &SourceLines {
        &self.source_lines
    }

    pub fn source_lines_mut(&mut self) -> &mut SourceLines {
        &mut self.source_lines
    }

    // AST metadata API //
    pub fn next_node_id(&mut self) -> NodeId {
        self.ast_metadata.next_node_id()
    }
}

pub struct WithSession<'a, T> {
    sess: &'a Session,
    val: &'a T,
}

pub struct StageResult<T> {
    sess: Session,
    data: T,
    messages: Vec<Message>,
}

pub type OkStageResult<T> = (T, Session);

impl<T> StageResult<T> {
    pub fn new(sess: Session, data: T, message_holder: MessageStorage) -> Self {
        Self {
            sess,
            data,
            messages: message_holder.extract(),
        }
    }

    pub fn unwrap(self) -> OkStageResult<T> {
        TermEmitter::new(&self.sess).emit(self.messages);

        (self.data, self.sess)
    }
}

pub trait Stage<T> {
    fn run(self) -> StageResult<T>;

    fn run_and_unwrap(self) -> OkStageResult<T>;
}
