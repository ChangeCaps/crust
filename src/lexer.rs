use thiserror::Error;

use crate::token::{Delim, Keyword, Symbol, Token, TokenType};

#[derive(Error, Debug, PartialEq)]
pub enum LexerError {
    #[error("expected token '{expected}' but found {found}")]
    MissingExpectedSymbol { expected: TokenType, found: Token },

    #[error("undefined token '{0}'")]
    Undefined(String),

    #[error("unexpected eof")]
    Eof,
}

pub type LexerResult<T> = Result<T, LexerError>;

struct PeakedToken {
	line: usize,
	column: usize,
    offset: usize,
    token: Token,
}

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

    fn lex_keyword(&mut self) -> LexerResult<Option<Keyword>> {
        let r = self.remaining().ok_or_else(|| LexerError::Eof)?;

        macro_rules! keyword {
			{
				$(
					$pat:literal => $keyword:expr,
				)*
			} => {
				$(
					if r.starts_with($pat) {
						self.consume_chars($pat.len())?;
						return Ok(Some($keyword));
					}
				)*
			}
		}

        keyword! {
            "let" => Keyword::Let,
        }

        Ok(None)
    }

    fn lex_symbol(&mut self) -> LexerResult<Option<Symbol>> {
        let r = self.remaining().ok_or_else(|| LexerError::Eof)?;

        macro_rules! symbol {
			{
				$(
					$pat:literal => $symbol:expr,
				)*
			} => {
				$(
					if r.starts_with($pat) {
						self.consume_chars($pat.len())?;
						return Ok(Some($symbol));
					}
				)*
			}
		}

        symbol! {
            "," => Symbol::Colon,
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

        Ok(None)
    }	

    pub fn next_token(&mut self) -> LexerResult<Token> {
        self.consume_whitespace_chars();

		let line = self.line;
		let column = self.column;
		let offset = self.offset;

        if let Some(keyword) = self.lex_keyword()? {
            return Ok(Token {
				line,
				column,
				offset,
				ty: TokenType::Keyword(keyword),
			});
        }

        if let Some(symbol) = self.lex_symbol()? {
            return Ok(Token {
				line,
				column,
				offset,
				ty: TokenType::Symbol(symbol),
			});
        }

        let ident = self.peek_chars_while(|c| c.is_alphanumeric() || c == '_');

		self.consume_chars(ident.len())?;

		if is_valid_ident(&ident) {
			Ok(Token {
				line,
				column,
				offset,
				ty: TokenType::Ident(ident),
			})
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
        let symbols = r#", ; == = && & || | += -= *= /= + - * / >= <= > < ( { [ ) } ]"#;

        let mut lexer = Lexer::new(symbols);

        macro_rules! assert_lex {
			{$($sym:expr),*} => {
				$(
					assert_eq!(lexer.next_token().unwrap().ty.to_string(), $sym);
				)*
			}
		}

        assert_lex!(
            ",", ";", "==", "=", "&&", "&", "||", "|", "+=", "-=", "*=", "/=", "+", "-", "*", "/",
            ">=", "<=", ">", "<", "(", "{", "[", ")", "}", "]"
        ); 
    }

	#[test]
	fn ident() {
		let idents = "foo Bar baz4 _test let";

		let mut lexer = Lexer::new(idents);

		assert_eq!(lexer.next_token().unwrap().ty, TokenType::Ident(String::from("foo")));
		assert_eq!(lexer.next_token().unwrap().ty, TokenType::Ident(String::from("Bar")));
		assert_eq!(lexer.next_token().unwrap().ty, TokenType::Ident(String::from("baz4")));
		assert_eq!(lexer.next_token().unwrap().ty, TokenType::Ident(String::from("_test")));
		assert_eq!(lexer.next_token().unwrap().ty, TokenType::Keyword(Keyword::Let));
	}
}
