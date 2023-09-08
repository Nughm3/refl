use std::{
    cmp::{max, min},
    fmt,
    ops::{Index, Range},
};

use text_size::TextRange;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    start: u32,
    end: u32,
}

impl Span {
    #[track_caller]
    #[inline(always)]
    pub fn new(start: u32, end: u32) -> Self {
        debug_assert!(start <= end, "spans must have the start <= end");
        Span { start, end }
    }

    #[inline(always)]
    pub fn start(&self) -> u32 {
        self.start
    }

    #[inline(always)]
    pub fn end(&self) -> u32 {
        self.end
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline(always)]
    pub fn contains(&self, other: Span) -> bool {
        self.start <= other.start && self.end > other.end
    }

    #[inline(always)]
    pub fn contains_offset(&self, offset: u32) -> bool {
        self.start <= offset && self.end > offset
    }

    #[inline]
    pub fn union(self, other: Span) -> Span {
        Span::new(min(self.start, other.start), max(self.end, other.end))
    }

    #[inline]
    pub fn intersection(self, other: Span) -> Option<Span> {
        let start = max(self.start, other.start);
        let end = min(self.end, other.end);
        (start <= end).then(|| Span::new(start, end))
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span::new(range.start as u32, range.end as u32)
    }
}

impl From<Span> for Range<usize> {
    fn from(Span { start, end }: Span) -> Self {
        start as usize..end as usize
    }
}

impl From<TextRange> for Span {
    fn from(range: TextRange) -> Self {
        Span::new(range.start().into(), range.end().into())
    }
}

impl From<Span> for TextRange {
    fn from(Span { start, end }: Span) -> Self {
        TextRange::new(start.into(), end.into())
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, Span { start, end }: Span) -> &Self::Output {
        &self[start as usize..end as usize]
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
