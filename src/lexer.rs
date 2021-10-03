use thiserror::Error;

use crate::{
    span::{Span, Spanned},
    token::{Delim, IntegerFormat, Keyword, Symbol, Token},
};

#[derive(Error, Debug, PartialEq)]
pub enum LexerError {
    #[error("undefined token '{0}'")]
    Undefined(String),

    #[error("floating point number with radix other than '10'")]
    InvalidFloatRadix,

    #[error("unexpected eof")]
    Eof,
}

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    pub line: usize,
    pub column: usize,

    pub offset: usize,

    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            line: 1,
            column: 1,
            offset: 0,
            source,
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.remaining()?.chars().next()
    }

    fn consume_char(&mut self) -> Option<char> {
        let c = self.remaining()?.chars().next();

        if let Some(c) = c {
            self.offset += 1;

            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }

        c
    }

    fn consume_chars(&mut self, len: usize) -> LexerResult<()> {
        for _ in 0..len {
            self.consume_char().ok_or_else(|| LexerError::Eof)?;
        }

        Ok(())
    }

    fn peek_chars_while(&mut self, f: impl Fn(char) -> bool) -> String {
        let mut out = String::new();

        let mut chars = if let Some(remaining) = self.remaining() {
            remaining.chars()
        } else {
            return out;
        };

        loop {
            if let Some(c) = chars.next() {
                if f(c) {
                    out.push(c);
                } else {
                    break out;
                }
            } else {
                break out;
            }
        }
    }

    fn consume_whitespace_chars(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.consume_char();
            } else {
                break;
            }
        }
    }

    fn remaining(&self) -> Option<&str> {
        if self.offset < self.source.len() {
            Some(&self.source[self.offset..])
        } else {
            None
        }
    }

    fn parse_number(&mut self) -> LexerResult<Option<Token>> {
        let mut lexer = self.clone();

        let negative = if let Some(c) = lexer.peek_char() {
            if c == '+' {
                lexer.consume_char();

                false
            } else if c == '-' {
                lexer.consume_char();

                true
            } else {
                false
            }
        } else {
            return Ok(None);
        };

        let mut radix = 10;

        if let Some(remaining) = lexer.remaining() {
            if remaining.starts_with("0x") {
                lexer.consume_chars(2)?;

                radix = 16;
            } else if remaining.starts_with("0b") {
                lexer.consume_chars(2)?;

                radix = 2;
            }
        }

        let first = lexer.peek_chars_while(|c| c.is_digit(radix));

        if first.len() > 0 {
            match self.peek_char() {
                Some('.') => {
                    if radix != 10 {
                        return Err(LexerError::InvalidFloatRadix);
                    }

                    let last = lexer.peek_chars_while(|c| c.is_digit(10));

                    let float = last.parse::<f32>().unwrap();

                    lexer.consume_chars(first.len() + 1 + last.len())?;

                    todo!()
                }
                _ => {
                    lexer.consume_chars(first.len())?;

                    // NOTE: will panic if 'first' represents an integer unrepresentable by a 64 bit
                    // integer
                    let mut int = i64::from_str_radix(&first, radix).unwrap();

                    if negative {
                        int = -int;
                    }

                    *self = lexer;

                    let format = match radix {
                        2 => IntegerFormat::Binary,
                        10 => IntegerFormat::Decimal,
                        16 => IntegerFormat::Hexadecimal,
                        _ => unreachable!(),
                    };

                    Ok(Some(Token::Integer(int, format)))
                }
            }
        } else {
            Ok(None)
        }
    }

    fn parse_keyword(&mut self) -> Option<Keyword> {
        let r = self.remaining()?;

        macro_rules! keyword {
			{
				$(
					$pat:literal => $keyword:expr,
				)*
			} => {
				$(
					if r.starts_with($pat) {
						self.consume_chars($pat.len()).unwrap();
						return Some($keyword);
					}
				)*
			}
		}

        keyword! {
            "let" => Keyword::Let,
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "loop" => Keyword::Loop,
            "while" => Keyword::While,
            "i32" => Keyword::I32,
            "f32" => Keyword::F32,
            "bool" => Keyword::Bool,
            "str"=> Keyword::Str,
            "void" => Keyword::Void,
        }

        None
    }

    fn parse_symbol(&mut self) -> Option<Symbol> {
        let r = self.remaining()?;

        macro_rules! symbol {
			{
				$(
					$pat:literal => $symbol:expr,
				)*
			} => {
				$(
					if r.starts_with($pat) {
						self.consume_chars($pat.len()).unwrap();
						return Some($symbol);
					}
				)*
			}
		}

        symbol! {
            "," => Symbol::Comma,
            "::" => Symbol::ColonColon,
            ":" => Symbol::Colon,
            ";" => Symbol::SemiColon,
            "==" => Symbol::EqEq,
            "=" => Symbol::Eq,
            "&&" => Symbol::AndAnd,
            "&" => Symbol::And,
            "||" => Symbol::OrOr,
            "|" => Symbol::Or,
            "+=" => Symbol::AddEq,
            "-=" => Symbol::SubEq,
            "*=" => Symbol::MulEq,
            "/=" => Symbol::DivEq,
            "+" => Symbol::Add,
            "-" => Symbol::Sub,
            "*" => Symbol::Mul,
            "/"=> Symbol::Div,
            "!=" => Symbol::NotEq,
            "!" => Symbol::Not,
            ">=" => Symbol::GtEq,
            "<=" => Symbol::LtEq,
            ">" => Symbol::Gt,
            "<" => Symbol::Lt,
            "(" => Symbol::Open(Delim::Paren),
            "[" => Symbol::Open(Delim::Bracket),
            "{" => Symbol::Open(Delim::Brace),
            ")" => Symbol::Close(Delim::Paren),
            "]" => Symbol::Close(Delim::Bracket),
            "}" => Symbol::Close(Delim::Brace),
        }

        None
    }

    pub fn next_token(&mut self) -> LexerResult<Spanned<Token>> {
        self.consume_whitespace_chars();

        let line = self.line;
        let column = self.column;
        let offset = self.offset;

        if self.remaining().is_none() {
            return Ok(Spanned {
                span: Span {
                    line,
                    column,
                    offset,
                    length: 0,
                },
                value: Token::Eof,
            });
        }

        if let Some(number) = self.parse_number()? {
            return Ok(Spanned {
                span: Span {
                    line,
                    column,
                    offset,
                    length: self.offset - offset,
                },
                value: number,
            });
        }

        if let Some(keyword) = self.parse_keyword() {
            return Ok(Spanned {
                span: Span {
                    line,
                    column,
                    offset,
                    length: self.offset - offset,
                },
                value: Token::Keyword(keyword),
            });
        }

        if let Some(symbol) = self.parse_symbol() {
            return Ok(Spanned {
                span: Span {
                    line,
                    column,
                    offset,
                    length: self.offset - offset,
                },
                value: Token::Symbol(symbol),
            });
        }

        let ident = self.peek_chars_while(|c| c.is_alphanumeric() || c == '_');

        self.consume_chars(ident.len())?;

        if is_valid_ident(&ident) {
            return Ok(Spanned {
                span: Span {
                    line,
                    column,
                    offset,
                    length: self.offset - offset,
                },
                value: Token::Ident(ident),
            });
        } else {
            Err(LexerError::Undefined(ident))
        }
    }
}

fn is_valid_ident(s: &str) -> bool {
    let mut chars = s.chars();

    if let Some(c) = chars.next() {
        if !(c.is_alphabetic() || c == '_') {
            return false;
        }

        for c in chars {
            if !(c.is_alphanumeric() || c == '_') {
                return false;
            }
        }

        true
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbols() {
        let symbols = r#", :: : ; == = && & || | += -= *= /= + - * / >= <= > < ( { [ ) } ]"#;

        let mut lexer = Lexer::new(symbols);

        macro_rules! assert_lex {
			{$($sym:expr),*} => {
				$(
					assert_eq!(lexer.next_token().unwrap().value.to_string(), $sym);
				)*
			}
		}

        assert_lex!(
            ",", "::", ":", ";", "==", "=", "&&", "&", "||", "|", "+=", "-=", "*=", "/=", "+", "-",
            "*", "/", ">=", "<=", ">", "<", "(", "{", "[", ")", "}", "]"
        );
    }

    #[test]
    fn ident() {
        let idents = "foo Bar baz4 _test let";

        let mut lexer = Lexer::new(idents);

        assert_eq!(
            lexer.next_token().unwrap().value,
            Token::Ident(String::from("foo"))
        );
        assert_eq!(
            lexer.next_token().unwrap().value,
            Token::Ident(String::from("Bar"))
        );
        assert_eq!(
            lexer.next_token().unwrap().value,
            Token::Ident(String::from("baz4"))
        );
        assert_eq!(
            lexer.next_token().unwrap().value,
            Token::Ident(String::from("_test"))
        );
        assert_eq!(
            lexer.next_token().unwrap().value,
            Token::Keyword(Keyword::Let)
        );
    }
}
