pub mod source;
pub mod sym;

use std::fmt::{Display, Formatter};

use self::source::{SourceId, DUMMY_SOURCE_ID};

pub type SpanPos = u32;
pub type SpanLen = u32;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Span {
    pos: SpanPos,
    len: SpanLen,
    source: SourceId,
}

impl Span {
    pub fn new_error() -> Self {
        Self {
            pos: 0,
            len: 1,
            source: DUMMY_SOURCE_ID,
        }
    }

    pub fn is_error(&self) -> bool {
        self.source == DUMMY_SOURCE_ID
    }

    pub fn new(pos: SpanPos, len: SpanLen, source: SourceId) -> Self {
        Self { pos, len, source }
    }

    pub fn new_file_top(source: SourceId) -> Self {
        Self {
            pos: 0,
            len: 1,
            source,
        }
    }

    pub fn lo(&self) -> SpanPos {
        self.pos
    }

    pub fn hi(&self) -> SpanPos {
        self.pos + self.len
    }

    pub fn len(&self) -> SpanLen {
        self.len
    }

    pub fn source(&self) -> SourceId {
        self.source
    }

    pub fn to(&self, end: Span) -> Self {
        assert!(self.source() == end.source());
        let lo = std::cmp::min(self.pos, end.pos);
        let hi = std::cmp::max(self.hi(), end.hi());
        assert!(hi >= lo);
        Span::new(lo, hi - lo, self.source())
    }

    pub fn point_before_lo(&self) -> Self {
        Self {
            pos: self.lo() - 1,
            len: 1,
            source: self.source(),
        }
    }

    pub fn point_after_hi(&self) -> Self {
        Self {
            pos: self.hi(),
            len: 1,
            source: self.source(),
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.lo(), self.hi())
    }
}

#[derive(Debug, Copy)]
pub struct Spanned<T> {
    span: Span,
    node: T,
}

impl<T> Clone for Spanned<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            span: self.span.clone(),
            node: self.node.clone(),
        }
    }
}

impl<T> Spanned<T> {
    pub fn new(span: Span, node: T) -> Self {
        Self { span, node }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn node(&self) -> &T {
        &self.node
    }
}

impl<T> Display for Spanned<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node())
    }
}

impl<T> WithSpan for Spanned<T> {
    fn span(&self) -> Span {
        self.span
    }
}

pub trait WithSpan {
    fn span(&self) -> Span;
}

impl<T> WithSpan for Box<T>
where
    T: WithSpan,
{
    fn span(&self) -> Span {
        self.as_ref().span()
    }
}

macro_rules! impl_with_span {
    ($ty: ident $(<$($generic: ident)+>)?) => {
        impl<$($($generic,)+)?> WithSpan for $ty<$($($generic,)+)?> {
            fn span(&self) -> Span {
                self.span
            }
        }
    };
}

pub(crate) use impl_with_span;
