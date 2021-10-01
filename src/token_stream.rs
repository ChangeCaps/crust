use std::rc::Rc;

use crate::{lexer::{Lexer, LexerResult}, token::{Token, TokenType}};

#[derive(Debug, Clone)]
pub struct TokenStream {
	idx: usize,
	tokens: Rc<Vec<Token>>,
}

impl TokenStream {
	pub fn new(source: &str) -> LexerResult<Self> {
		let mut tokens = Vec::new();
		
		let mut lexer = Lexer::new(source);

		loop {
			let token = lexer.next_token()?;

			if let TokenType::Eof = token.ty {
				tokens.push(token);

				break Ok(Self {
					idx: 0,
					tokens: Rc::new(tokens),
				});
			} else {
				tokens.push(token);
			}
		}
	} 	

	pub fn next(&mut self) -> &Token {
		let idx = self.idx.min(self.tokens.len() - 1);
		let token = &self.tokens[idx];
		self.idx += 1;

		token
	}
}