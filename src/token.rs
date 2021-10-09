use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// Comment
    Comment(String),
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

impl std::fmt::Display for Token {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(comment) => write!(f, "//{}", comment),
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::Integer(int, format) => match format {
                IntegerFormat::Binary => write!(f, "{:b}", int),
                IntegerFormat::Decimal => write!(f, "{}", int),
                IntegerFormat::Hexadecimal => {
                    if *int < 0 {
                        write!(f, "-{:X}", int.abs())
                    } else {
                        write!(f, "{:X}", int.abs())
                    }
                }
            },
            Self::Symbol(symbol) => write!(f, "{}", symbol),
            Self::Keyword(keyword) => write!(f, "{}", keyword),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

impl Token {
    #[inline]
    pub fn ty(&self) -> TokenType {
        match self {
            Self::Comment(_) => TokenType::Comment,
            Self::Ident(ident) => TokenType::SpecificIdent(ident.clone().into()),
            Self::Integer(int, _format) => TokenType::SpecificInteger(*int),
            Self::Symbol(symbol) => TokenType::SpecificSymbol(*symbol),
            Self::Keyword(keyword) => TokenType::SpecificKeyword(*keyword),
            Self::Eof => TokenType::Eof,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Comment,
    Ident,
    SpecificIdent(Cow<'static, str>),
    Integer,
    SpecificInteger(i64),
    Symbol,
    SpecificSymbol(Symbol),
    Keyword,
    SpecificKeyword(Keyword),
    Eof,
}

impl std::fmt::Display for TokenType {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment => write!(f, "{{comment}}"),
            Self::Ident => write!(f, "{{ident}}"),
            Self::SpecificIdent(ident) => write!(f, "{}", ident),
            Self::Integer => write!(f, "{{integer}}"),
            Self::SpecificInteger(int) => write!(f, "{}", int),
            Self::Symbol => write!(f, "{{symbol}}"),
            Self::SpecificSymbol(symbol) => write!(f, "{}", symbol),
            Self::Keyword => write!(f, "{{keyword}}"),
            Self::SpecificKeyword(keyword) => write!(f, "{}", keyword),
            Self::Eof => write!(f, "{{EOF}}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntegerFormat {
    Binary,
    Decimal,
    Hexadecimal,
}

/// Delimiters '(', '{' and '['.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Symbol {
    Comma,
    ColonColon,
    Colon,
    SemiColon,
    Arrow,
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
    NotEq,
    Not,
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
            Self::Comma => write!(f, ","),
            Self::ColonColon => write!(f, "::"),
            Self::Colon => write!(f, ":"),
            Self::SemiColon => write!(f, ";"),
            Self::Arrow => write!(f, "->"),
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
            Self::NotEq => write!(f, "!="),
            Self::Not => write!(f, "!"),
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Let,
    If,
    Else,
    Loop,
    While,
    I32,
    F32,
    Bool,
    Str,
    Void,
    True,
    False,
    Fn,
    Return,
    Mod,
    Use,
    Super,
}

impl std::fmt::Display for Keyword {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Let => "let",
                Self::If => "if",
                Self::Else => "else",
                Self::Loop => "loop",
                Self::While => "while",
                Self::I32 => "i32",
                Self::F32 => "f32",
                Self::Bool => "bool",
                Self::Str => "str",
                Self::Void => "void",
                Self::True => "true",
                Self::False => "false",
                Self::Fn => "fn",
                Self::Return => "return",
                Self::Mod => "mod",
                Self::Use => "use",
                Self::Super => "super",
            }
        )
    }
}
