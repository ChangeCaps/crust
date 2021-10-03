#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathSegment {
    Ident(String),
}

impl std::fmt::Display for PathSegment {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => ident.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    pub fn join(&self, segment: impl Into<String>) -> Self {
        let mut new = self.clone();
        new.segments.push(PathSegment::Ident(segment.into()));
        new
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
