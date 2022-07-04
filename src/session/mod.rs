use crate::{
    ast::{AstMetadata, NodeId},
    config::config::Config,
    message::{
        message::{Message, MessageStorage},
        term_emitter::TermEmitter,
        MessageEmitter,
    },
    resolve::{def::DefTable, res::Resolutions},
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
#[derive(Debug)]
pub struct SpanSourceInfo<'a> {
    pub lines: Vec<LineInfo<'a>>,
    pub pos_in_line: SpanPos,
}

#[derive(Debug)]
pub struct LineInfo<'a> {
    pub str: &'a str,
    pub index: usize,                   // Line index in Source.lines_positions
    pub prev_line_index: Option<usize>, // Index of previous line
    pub pos: SpanPos,                   // Line absolute source position
    pub num: usize,                     // Line number (starts with 1)
    pub num_len: usize,                 // Length of line number as string
    pub num_indent: usize, // Calculated indent for line number to align with source lines
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

    /// Find line index by span
    fn find_span_lines(&self, span: Span) -> Vec<usize> {
        if span.is_error() {
            panic!()
        }

        let mut indices = vec![];

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

            let line_range = line_pos..=next_line_pos;

            if line_range.contains(&span.lo()) || line_range.contains(&span.hi()) {
                indices.push(i);
            }
        }

        assert!(!indices.is_empty());

        indices
    }

    pub fn get_line_info<'a>(&'a self, index: usize) -> LineInfo<'a> {
        let lines_count = self.lines_count();

        let prev_line_index = if index > 0 { Some(index - 1) } else { None };

        let next_line_pos = *self
            .lines_positions
            .get(index + 1)
            .unwrap_or(&(self.source_size() as SpanPos));

        let pos = *self.lines_positions.get(index).unwrap();
        let line = &self.source[pos as usize..(next_line_pos - 1) as usize];
        let num = index + 1;
        let num_len = num.to_string().len();
        let num_indent = num_len - (lines_count + 1).to_string().len();

        LineInfo {
            str: line,
            prev_line_index,
            index,
            pos,
            num,
            num_len,
            num_indent,
        }
    }

    pub fn get_span_info<'a>(&'a self, span: Span) -> SpanSourceInfo<'a> {
        let lines = self
            .find_span_lines(span)
            .iter()
            .map(|index| self.get_line_info(*index))
            .collect::<Vec<_>>();

        let line_pos = lines.first().unwrap().pos;

        SpanSourceInfo {
            lines,
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
                    *next_pos as usize - 1
                } else {
                    self.source_size()
                };

                &self.source()[pos..next_pos]
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
    pub def_table: DefTable,
    pub res: Resolutions,
}

impl Session {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            source_map: Default::default(),
            ast_metadata: Default::default(),
            def_table: Default::default(),
            res: Resolutions::default(),
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
    pub fn new(sess: Session, data: T, messages: MessageStorage) -> Self {
        Self {
            sess,
            data,
            messages: messages.extract(),
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
