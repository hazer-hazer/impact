use core::fmt;
use std::fmt::Display;

use crate::{
    cli::color::Color,
    span::span::{Ident, Span},
};

#[derive(Clone, Copy, Debug)]
pub enum NameKind {
    Var,
    Func,
    Const,
    File,
    Variant,
    Type,
    TypeVar,
    Mod,
}

impl Display for NameKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NameKind::Var => "variable",
                NameKind::Func => "function",
                NameKind::Const => "constant",
                NameKind::File => "file",
                NameKind::Variant => "enum variant",
                NameKind::Type => "type",
                NameKind::TypeVar => "type variable",
                NameKind::Mod => "module",
            }
        )
    }
}

/// For LS
#[derive(Debug)]
pub enum SolutionKind {
    Rename {
        kind: NameKind,
        name: Ident,
        to: Option<String>,
        // TODO: Name identifier for LS
    },
}

#[derive(Debug)]
pub struct Solution {
    kind: SolutionKind,
}

impl Solution {
    pub fn new(kind: SolutionKind) -> Self {
        Self { kind }
    }

    pub fn kind(&self) -> &SolutionKind {
        &self.kind
    }
}

#[derive(PartialEq, Debug)]
pub enum MessageKind {
    Error,
    Warn,
}

impl Display for MessageKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MessageKind::Error => "error",
                MessageKind::Warn => "warning",
            }
        )
    }
}

impl MessageKind {
    pub fn color(&self) -> Color {
        match self {
            MessageKind::Error => Color::BrightRed,
            MessageKind::Warn => Color::Yellow,
        }
    }
}

#[derive(Debug)]
pub struct Message {
    kind: MessageKind,

    // The header of the message, e.g. "Error blah-blah at 1:3"
    span: Span,
    text: String,

    // Labels are spanned
    labels: Vec<Label>,

    solution: Option<Solution>,

    origin: Option<String>,
}

impl Message {
    pub fn is(&self, kind: MessageKind) -> bool {
        self.kind == kind
    }

    pub fn kind(&self) -> &MessageKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn text(&self) -> &str {
        self.text.as_ref()
    }

    pub fn labels(&self) -> &[Label] {
        self.labels.as_ref()
    }

    pub fn solution(&self) -> Option<&Solution> {
        self.solution.as_ref()
    }

    pub fn origin(&self) -> Option<&String> {
        self.origin.as_ref()
    }
}

#[derive(Debug)]
pub struct Label {
    span: Span,
    text: String,
}

impl Label {
    pub fn new(span: Span, text: String) -> Self {
        Self { span, text }
    }

    pub fn text(&self) -> &str {
        self.text.as_ref()
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

pub trait MessageHolder {
    fn save(&mut self, msg: Message);
}

// FIXME: Rename 'cause of MsgResult
pub type MessagesResult<T> = Result<T, MessageStorage>;

#[derive(Default)]
pub struct MessageStorage {
    messages: Vec<Message>,
}

impl MessageStorage {
    pub fn add_message(&mut self, msg: Message) {
        self.messages.push(msg);
    }

    pub fn merge(&mut self, other: MessageStorage) {
        self.messages.extend(other.messages);
    }

    pub fn extract(self) -> Vec<Message> {
        self.messages
    }

    pub fn error_checked_result<T>(self, value: T) -> MessagesResult<T> {
        if self.messages.iter().any(|msg| msg.is(MessageKind::Error)) {
            Err(self)
        } else {
            Ok(value)
        }
    }
}

pub type MsgResult<T> = Result<T, MessageBuilder>;

pub struct MessageBuilder {
    kind: MessageKind,
    span: Option<Span>,
    text: Option<String>,
    labels: Vec<Label>,
    solution: Option<Solution>,
    origin: Option<String>,
}

impl MessageBuilder {
    pub fn of_kind(kind: MessageKind) -> Self {
        Self {
            kind,
            span: None,
            text: None,
            labels: vec![],
            solution: None,
            origin: None,
        }
    }

    pub fn error() -> MessageBuilder {
        MessageBuilder::of_kind(MessageKind::Error)
    }

    pub fn warn() -> MessageBuilder {
        MessageBuilder::of_kind(MessageKind::Warn)
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn text(mut self, text: String) -> Self {
        self.text = Some(text);
        self
    }

    pub fn label(mut self, span: Span, text: String) -> Self {
        self.labels.push(Label { span, text });
        self
    }

    pub fn solution(mut self, solution: Solution) -> Self {
        self.solution = Some(solution);
        self
    }

    pub fn origin(mut self, file: &str, line: u32) -> Self {
        self.origin = Some(format!("{}:{}", file, line));
        self
    }

    pub fn build(self) -> Message {
        let span = self.checked_span();
        let text = self.checked_text().clone();
        Message {
            kind: self.kind,
            span,
            text,
            labels: self.labels,
            solution: self.solution,
            origin: self.origin,
        }
    }

    fn checked_span(&self) -> Span {
        self.span
            .expect("Tried to create message without span specified")
    }

    fn checked_text(&self) -> String {
        self.text
            .as_ref()
            .expect("Tried to create message without text specified")
            .clone()
    }

    /// If no label were supplied, we use message's span
    ///  and text for the single label to actually point user to some place in the code
    pub fn emit_single_label(self, holder: &mut impl MessageHolder) {
        assert!(self.labels.is_empty());
        let span = self.checked_span();
        let text = self.checked_text();
        holder.save(self.label(span, text).build());
    }

    pub fn emit(self, holder: &mut impl MessageHolder) {
        assert!(!self.labels.is_empty());
        holder.save(self.build());
    }
}
