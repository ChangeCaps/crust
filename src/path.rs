#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathSegment {
    Ident(String),
    Super,
    WildCard,
}

impl std::fmt::Display for PathSegment {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => ident.fmt(f),
            Self::Super => write!(f, "super"),
            Self::WildCard => write!(f, "*"),
        }
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

impl Path {
    #[inline]
    pub fn ident(segment: impl Into<String>) -> Self {
        Self {
            segments: vec![PathSegment::Ident(segment.into())],
        }
    }

    #[inline]
    pub fn join(&self, segment: impl Into<PathSegment>) -> Self {
        let mut new = self.clone();
        new.segments.push(segment.into());
        new
    }

    #[inline]
    pub fn combine(&self, other: &Self) -> Self {
        let mut path = self.clone();

        path.segments.append(&mut other.segments.clone());

        path
    }
}

impl Into<PathSegment> for &str {
    #[inline]
    fn into(self) -> PathSegment {
        PathSegment::Ident(self.into())
    }
}

impl Into<PathSegment> for &String {
    #[inline]
    fn into(self) -> PathSegment {
        PathSegment::Ident(self.into())
    }
}

impl Into<PathSegment> for String {
    #[inline]
    fn into(self) -> PathSegment {
        PathSegment::Ident(self)
    }
}

impl std::fmt::Display for Path {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut segments = self.segments.iter();

        if let Some(first) = segments.next() {
            write!(f, "{}", first)?;

            for segment in segments {
                write!(f, "::{}", segment)?;
            }

            Ok(())
        } else {
            write!(f, "::")
        }
    }
}
