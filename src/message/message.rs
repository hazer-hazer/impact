use crate::span::span::Span;

#[derive(PartialEq, Debug)]
pub enum MessageKind {
    Error,
    Warn,
}

#[derive(Debug,)]
pub struct Message {
    kind: MessageKind,
    span: Span,
    text: String,
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
}

pub struct MessageBuilder {
    kind: MessageKind,
    span: Option<Span>,
    text: Option<String>,
}

impl MessageBuilder {
    pub fn of_kind(kind: MessageKind) -> Self {
        Self {
            kind,
            span: None,
            text: None,
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

    pub fn build(self) -> Message {
        Message {
            kind: self.kind,
            span: self
                .span
                .expect("Tried to create message without span specified"),
            text: self
                .text
                .expect("Tried to create message without text specified"),
        }
    }

    pub fn emit(self, holder: &mut impl MessageHolder) {
        holder.save(self.build());
    }
}
