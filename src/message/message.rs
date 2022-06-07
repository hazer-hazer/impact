use crate::{span::span::Span};


#[derive(PartialEq, Debug)]
pub enum MessageKind {
    Error,
    Warn,
}

#[derive(Debug)]
pub struct Message {
    kind: MessageKind,
    span: Span,
    text: Option<String>,
}

#[derive(Default)]
pub struct MessageHolder {
    messages: Vec<Message>,
}

impl MessageHolder {
    pub fn add_message(&mut self, msg: Message) {
        self.messages.push(msg);
    }

    pub fn extract(self) -> Vec<Message> {
        self.messages
    }
}

impl Message {
    // Building //
    fn of_kind(kind: MessageKind, span: Span) -> Self {
        Self {
            kind,
            span,
            text: None,
        }
    }

    pub fn error(span: Span) -> Self {
        Message::of_kind(MessageKind::Error, span)
    }

    pub fn warn(span: Span) -> Self {
        Message::of_kind(MessageKind::Warn, span)
    }

    pub fn text(&mut self, text: String) -> &mut Self {
        self.text = Some(text);
        self
    }

    pub fn is(&self, kind: MessageKind) -> bool {
        self.kind == kind
    }

    // Emitting //
    pub fn emit<T>(self, holder: &mut MessageHolder) {
        holder.add_message(self)
    }
}
