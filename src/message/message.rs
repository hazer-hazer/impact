use std::fmt::Display;

use crate::{
    cli::color::Color,
    span::span::{Ident, Span},
};

/// For LS
pub enum SolutionKind {
    Rename {
        name: Ident,
        to: String,
        // TODO: Name identifier for LS
    },
}

pub struct Solution {
    kind: SolutionKind,
}

impl Solution {
    pub fn new(kind: SolutionKind) -> Self {
        Self { kind }
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
}

#[derive(Debug)]
pub struct Label {
    span: Span,
    text: String,
}

impl Label {
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

#[derive(Default)]
pub struct MessageStorage {
    messages: Vec<Message>,
}

impl MessageStorage {
    pub fn add_message(&mut self, msg: Message) {
        self.messages.push(msg);
    }

    pub fn extract(self) -> Vec<Message> {
        self.messages
    }
}

pub struct MessageBuilder {
    kind: MessageKind,
    span: Option<Span>,
    text: Option<String>,
    labels: Vec<Label>,
}

impl MessageBuilder {
    pub fn of_kind(kind: MessageKind) -> Self {
        Self {
            kind,
            span: None,
            text: None,
            labels: vec![],
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

    pub fn build(self) -> Message {
        let span = self.checked_span();
        let text = self.checked_text().clone();
        Message {
            kind: self.kind,
            span,
            text,
            labels: self.labels,
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
