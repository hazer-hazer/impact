use crate::{
    cli::color::{Color, WithColor},
    dt::idx::{declare_idx, Idx},
    span::{Span, SpanPos},
};

declare_idx!(SourceId, u32, "source[{}]", Color::White);

pub const DUMMY_SOURCE_ID: SourceId = SourceId(u32::MAX);

/// Computed information about line, use not for storage but as a helper
#[derive(Debug)]
pub struct SpanSourceInfo<'a> {
    pub lines: Vec<LineInfo<'a>>,
    pub pos_in_line: SpanPos,
}

#[derive(Debug, Clone, Copy)]
pub struct LineInfo<'a> {
    pub str: &'a str,
    /// Line index in Source.lines_positions
    pub index: usize,
    /// Index of previous line
    pub prev_line_index: Option<usize>,
    /// Line absolute source position
    pub pos: SpanPos,
    /// Line number (starts with 1)
    pub num: usize,
    /// Length of line number as string
    pub num_len: usize,
    /// Calculated indent for line number to align with
    /// source lines
    pub num_indent: usize,
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

            // We encountered line further than span
            if span.hi() < line_pos {
                break;
            }

            let next_line_pos = *self
                .lines_positions
                .get(i + 1)
                .unwrap_or(&(self.source_size() as u32));

            if next_line_pos <= span.lo() {
                // Do not include line if span starts on the next line
                continue;
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
            // Note: Add 1 as we subtract 1 in range
            .unwrap_or(&(self.source_size() as SpanPos + 1));

        let pos = *self.lines_positions.get(index).unwrap();
        let line = &self.source[pos as usize..(next_line_pos - 1) as usize];
        let num = index + 1;
        let num_len = num.to_string().len();
        let num_indent = (lines_count + 1).to_string().len() - num_len;

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
    // FIXME: Use IndexVec
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
