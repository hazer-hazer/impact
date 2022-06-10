use string_interner::StringInterner;

use crate::{
    message::{
        message::{Message, MessageStorage},
        term_emitter::TermEmitter,
        MessageEmitter,
    },
    span::span::{Kw, Span, SpanPos, Symbol},
};

/**
 * Session is a compilation context passed through all stages of compilation.
 */

#[derive(Default)]
pub struct SourceLines {
    lines: Vec<String>,
    positions: Vec<SpanPos>,
}

impl SourceLines {
    pub fn add_line(&mut self, line: String, pos: SpanPos) {
        self.lines.push(line);
        self.positions.push(pos);
    }

    pub fn find_line(&self, span: Span) -> (&String, SpanPos) {
        let mut line = None;
        let mut pos = None;

        for i in 0..self.lines.len() {
            let line_pos = self.positions[i];

            // We encountered line further than span
            if span.pos < line_pos {
                break;
            }

            if span.pos >= line_pos {
                line = Some(&self.lines[i]);
                pos = Some(line_pos);
                break;
            }
        }

        (line.unwrap(), pos.unwrap())
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
    interner: StringInterner,
    source_lines: SourceLines,
}

pub struct WithSession<'a, T> {
    sess: &'a Session,
    val: &'a T,
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

    // Interner API //
    pub fn intern<T>(&mut self, string: T) -> Symbol
    where
        T: AsRef<str>,
    {
        Symbol::new(self.interner.get_or_intern(string))
    }

    pub fn get_str(&self, sym: Symbol) -> &str {
        self.interner
            .resolve(sym.as_inner())
            .expect(format!("Failed to resolve symbol {sym:?}").as_str())
    }

    pub fn as_kw(&self, sym: Symbol) -> Option<Kw> {
        match self.get_str(sym) {
            "let" => Some(Kw::Let),
            _ => None,
        }
    }
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
