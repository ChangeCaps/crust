use thiserror::Error;

use crate::{
    expr::{BinOp, Expr, Literal},
    path::{Path, PathSegment},
    span::Spanned,
    token::{Delim, Symbol, Token, TokenType},
    token_stream::TokenStream,
};

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("expected '{expected}' but found {found}")]
    MissingExpectedToken {
        expected: TokenType,
        found: Spanned<Token>,
    },

    #[error("")]
    ExpectedOneTokens {
        expected: &'static [&'static TokenType],
        found: Spanned<Token>,
    },

    #[error("found unexpected token {0}")]
    Unexpected(Spanned<Token>),
}

pub type ParserResult<T> = Result<T, ParserError>;

fn expect(tokens: &mut TokenStream, token: &Token) -> ParserResult<()> {
    if tokens.next().value == *token {
        Ok(())
    } else {
        Err(ParserError::MissingExpectedToken {
            expected: token.ty(),
            found: tokens.next().clone(),
        })
    }
}

fn parse_path_segment(tokens: &mut TokenStream) -> ParserResult<PathSegment> {
    let tok = tokens.next();

    match tok.value {
        Token::Ident(ref ident) => Ok(PathSegment::Ident(ident.clone())),
        _ => Err(ParserError::ExpectedOneTokens {
            expected: &[&TokenType::Ident],
            found: tok.clone(),
        }),
    }
}

fn parse_path(tokens: &mut TokenStream) -> ParserResult<Path> {
    let mut segments = vec![parse_path_segment(tokens)?];

    loop {
        if let Token::Symbol(Symbol::ColonColon) = tokens.peek().value {
            tokens.consume();

            segments.push(parse_path_segment(tokens)?);
        } else {
            break Ok(Path { segments });
        }
    }
}

fn parse_term(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let tok = tokens.peek();

    match tok.value {
        Token::Integer(int, format) => {
            tokens.consume();

            Ok(Expr::Literal(Literal::Integer(int, format)))
        }
        Token::Ident(_) => {
            let path = parse_path(tokens)?;

            Ok(Expr::Path(path))
        }
        Token::Symbol(Symbol::Open(Delim::Paren)) => {
            tokens.consume();

            let expr = parse_expr(tokens)?;

            expect(tokens, &Token::Symbol(Symbol::Close(Delim::Paren)))?;

            Ok(Expr::Paren(Box::new(expr)))
        }
        _ => Err(ParserError::Unexpected(tok.clone())),
    }
}

fn parse_unary(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let tok = tokens.peek();

    match tok.value {
        _ => parse_term(tokens),
    }
}

fn parse_factor(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let lhs = parse_unary(tokens)?;
    let tok = tokens.peek();

    match tok.value {
        Token::Symbol(symbol @ Symbol::Mul) | Token::Symbol(symbol @ Symbol::Div) => {
            tokens.consume();

            let rhs = parse_factor(tokens)?;

            Ok(Expr::Binary(
                Box::new(lhs),
                BinOp::from_symbol(&symbol).unwrap(),
                Box::new(rhs),
            ))
        }
        _ => Ok(lhs),
    }
}

fn parse_additive(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let lhs = parse_factor(tokens)?;
    let tok = tokens.peek();

    match tok.value {
        Token::Symbol(symbol @ Symbol::Add) | Token::Symbol(symbol @ Symbol::Sub) => {
            tokens.consume();

            let rhs = parse_factor(tokens)?;

            Ok(Expr::Binary(
                Box::new(lhs),
                BinOp::from_symbol(&symbol).unwrap(),
                Box::new(rhs),
            ))
        }
        _ => Ok(lhs),
    }
}

fn parse_assign(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let lhs = parse_additive(tokens)?;
    let tok = tokens.peek();

    match tok.value {
        Token::Symbol(Symbol::Eq) => {
            tokens.consume();

            let rhs = parse_assign(tokens)?;

            Ok(Expr::Assign(Box::new(lhs), Box::new(rhs)))
        }
        _ => Ok(lhs),
    }
}

fn parse_expr(tokens: &mut TokenStream) -> ParserResult<Expr> {
    parse_assign(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_parsed {
        {$fn:path, $str:literal} => {
            {
                let mut tokens = TokenStream::new($str).unwrap();

                assert_eq!($fn(&mut tokens).unwrap().to_string(), $str);
            }
        }
    }

    #[test]
    fn expr() {
        assert_parsed!(parse_expr, "-0x31");
        assert_parsed!(parse_expr, "10");
        assert_parsed!(parse_expr, "4 * 0b101");
        assert_parsed!(parse_expr, "4 * 2 / 5");
        assert_parsed!(parse_expr, "4 * 2 / 5 + 2");
        assert_parsed!(parse_expr, "4 * (2 + 5) / 2");
        assert_parsed!(parse_expr, "x = 2");
        assert_parsed!(parse_expr, "x = foo::bar::BAZ / 2");
    }
}
