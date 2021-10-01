#[derive(Debug, PartialEq)]
pub struct Token {
	pub line: usize,
	pub column: usize,
	pub offset: usize,
	pub ty: TokenType,
}

impl std::fmt::Display for Token {
	#[inline]
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "'{}' at line: {}, column: {}", self.ty, self.line, self.column)	
	}
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    /// Identifier and sequence of text that isn't a keyword.
    Ident(String),
    /// Integer, see [`IntegerFormat`].
    Integer(i64, IntegerFormat),
    /// Symbol eg. '&', '+=' and '/'.
    Symbol(Symbol),
    /// Keyword eg. 'let'
    Keyword(Keyword),
    /// End of file.
    Eof,
}

impl std::fmt::Display for TokenType {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::Integer(int, format) => match format {
                IntegerFormat::Binary => write!(f, "{:b}", int),
                IntegerFormat::Decimal => write!(f, "{}", int),
                IntegerFormat::Hexadecimal => write!(f, "{:x}", int),
            },
            Self::Symbol(symbol) => write!(f, "{}", symbol),
            Self::Keyword(keyword) => write!(f, "{}", keyword),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum IntegerFormat {
    Binary,
    Decimal,
    Hexadecimal,
}

/// Delimiters '(', '{' and '['.
#[derive(Debug, PartialEq)]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Colon,
    SemiColon,
    EqEq,
    Eq,
    AndAnd,
    And,
    OrOr,
    Or,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    Add,
    Sub,
    Mul,
    Div,
    GtEq,
    LtEq,
    Gt,
    Lt,
    Open(Delim),
    Close(Delim),
}

impl std::fmt::Display for Symbol {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Colon => write!(f, ","),
            Self::SemiColon => write!(f, ";"),
            Self::EqEq => write!(f, "=="),
            Self::Eq => write!(f, "="),
            Self::AndAnd => write!(f, "&&"),
            Self::And => write!(f, "&"),
            Self::OrOr => write!(f, "||"),
            Self::Or => write!(f, "|"),
            Self::AddEq => write!(f, "+="),
            Self::SubEq => write!(f, "-="),
            Self::MulEq => write!(f, "*="),
            Self::DivEq => write!(f, "/="),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::GtEq => write!(f, ">="),
            Self::LtEq => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::Open(Delim::Paren) => write!(f, "("),
            Self::Open(Delim::Brace) => write!(f, "{{"),
            Self::Open(Delim::Bracket) => write!(f, "["),
            Self::Close(Delim::Paren) => write!(f, ")"),
            Self::Close(Delim::Brace) => write!(f, "}}"),
            Self::Close(Delim::Bracket) => write!(f, "]"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Let,
}

impl std::fmt::Display for Keyword {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let => write!(f, "let"),
        }
    }
}
