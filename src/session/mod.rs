use crate::{
    ast::{AstMetadata, NodeId},
    config::config::Config,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SourceId(u32);

pub const DUMMY_SOURCE_ID: SourceId = SourceId(u32::MAX);

impl SourceId {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn is_dummy(&self) -> bool {
        *self == DUMMY_SOURCE_ID
    }
}

/// Computed information about line, use not for storage but as a helper
pub struct SpanSourceInfo<'a> {
    pub line: &'a str,
    pub line_pos: SpanPos,
    pub line_size: usize,
    pub line_num: usize,
    pub line_num_len: usize,
    pub line_num_indent: usize,
    pub pos_in_line: SpanPos,
}

/// Some source, e.g. source file
pub struct Source {
    filename: String,
    source: String,
    lines_positions: Vec<SpanPos>,
}

impl Source {
    pub fn new(filename: String, source: String) -> Self {
        Self {
            filename,
            source,
            lines_positions: vec![],
        }
    }

    pub fn add_line(&mut self, pos: SpanPos) {
        self.lines_positions.push(pos)
    }

    pub fn source_size(&self) -> usize {
        self.source.len()
    }

    pub fn lines_count(&self) -> usize {
        self.lines_positions.len()
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn filename(&self) -> &str {
        self.filename.as_ref()
    }

    /// get (line string, line position, line number)
    pub fn find_line(&self, span: Span) -> (&str, SpanPos, usize) {
        if span.is_error() {
            panic!()
        }

        for i in 0..self.lines_positions.len() {
            let line_pos = self.lines_positions[i];
            let next_line_pos = *self
                .lines_positions
                .get(i + 1)
                .unwrap_or(&(self.source_size() as u32));

            // We encountered line further than span
            if span.lo() < line_pos {
                break;
            }

            if span.lo() >= line_pos && span.lo() < next_line_pos {
                return (
                    &self.source[line_pos as usize..next_line_pos as usize],
                    line_pos,
                    i + 1,
                );
            }
        }

        panic!("No source line found for span {}", span);
    }

    pub fn get_line_info<'a>(&self, sess: &'a Session, span: Span) -> SpanSourceInfo<'a> {
        let source = sess.source_map.get_source(span.source());

        let lines_count = source.lines_count();

        let (line, line_pos, line_num) = if span.is_error() {
            ("[The place my stupid mistakes live]", 0, 0)
        } else {
            let (line, pos, num) = source.find_line(span);
            assert!(pos <= span.lo());

            (line, pos, num)
        };

        // FIXME: Optimize
        let line_num_len = line_num.to_string().len();

        let line_num_indent = line_num_len - (lines_count + 1).to_string().len();

        SpanSourceInfo {
            line,
            line_pos,
            line_size: line.len(),
            line_num,
            line_num_len,
            line_num_indent,
            pos_in_line: span.lo() - line_pos,
        }
    }

    pub fn get_lines(&self) -> Vec<&str> {
        self.lines_positions
            .iter()
            .enumerate()
            .map(|(index, pos)| {
                let pos = *pos as usize;
                let next_pos = if let Some(next_pos) = self.lines_positions.get(index + 1) {
                    *next_pos as usize
                } else {
                    self.source_size()
                };

                &self.source()[pos..(next_pos - 1)]
            })
            .collect::<Vec<_>>()
    }

    pub fn lines_positions(&self) -> &[u32] {
        self.lines_positions.as_ref()
    }
}

#[derive(Default)]
pub struct SourceMap {
    sources: Vec<Source>,
}

impl SourceMap {
    pub fn add_source(&mut self, source: Source) -> SourceId {
        let source_id = SourceId(self.sources.len() as u32);
        self.sources.push(source);
        source_id
    }

    pub fn get_source(&self, source_id: SourceId) -> &Source {
        self.sources
            .get(source_id.as_usize())
            .expect(format!("Failed to get source by {:?}", source_id).as_str())
    }

    pub fn get_source_mut(&mut self, source_id: SourceId) -> &mut Source {
        self.sources.get_mut(source_id.as_usize()).unwrap()
    }
}

pub struct Session {
    config: Config,
    pub source_map: SourceMap,
    ast_metadata: AstMetadata,
}

impl Session {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            source_map: Default::default(),
            ast_metadata: Default::default(),
        }
    }

    pub fn with_sess<'a, T>(&'a self, val: &'a T) -> WithSession<'a, T> {
        WithSession {
            sess: self,
            val: val,
        }
    }

    pub fn config(&self) -> &Config {
        &self.config
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

pub struct StageOutput<T> {
    pub sess: Session,
    pub data: T,
    pub messages: Vec<Message>,
}

pub type StageResult<T> = Result<(T, Session), String>;

impl<T> StageOutput<T> {
    pub fn new(sess: Session, data: T, message_holder: MessageStorage) -> Self {
        Self {
            sess,
            data,
            messages: message_holder.extract(),
        }
    }

    pub fn emit(self, stop_on_error: bool) -> StageResult<T> {
        TermEmitter::new().emit(self, stop_on_error)
    }
}

pub trait Stage<T>
where
    Self: Sized,
{
    fn run(self) -> StageOutput<T>;

    fn run_and_emit(self, stop_on_error: bool) -> StageResult<T> {
        let output = self.run();
        output.emit(stop_on_error)
    }
}
