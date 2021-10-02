use std::{
    cmp::Ordering,
    ops::{Deref, DerefMut},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
    pub length: usize,
}

impl std::ops::BitOr for Span {
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        match self.offset.cmp(&rhs.offset) {
            Ordering::Less => Self {
                line: self.line,
                column: self.column,
                offset: self.offset,
                length: (rhs.offset - self.offset + rhs.length).max(self.length),
            },
            Ordering::Equal => Self {
                line: self.line,
                column: self.column,
                offset: self.offset,
                length: self.length.max(rhs.offset),
            },
            Ordering::Greater => Self {
                line: rhs.line,
                column: rhs.column,
                offset: rhs.offset,
                length: (self.offset - rhs.offset + self.length).max(rhs.length),
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    #[inline]
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "'{}' at line: {}, column: {}",
            self.value, self.span.line, self.span.column
        )
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
