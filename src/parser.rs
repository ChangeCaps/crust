use thiserror::Error;

use crate::{
    expr::{BinOp, Block, Expr, Literal, UnaryOp},
    path::{Path, PathSegment},
    span::Spanned,
    token::{Delim, Keyword, Symbol, Token, TokenType},
    token_stream::TokenStream,
    value::Type,
};

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("expected '{expected}' but found {found}")]
    MissingExpectedToken {
        expected: TokenType,
        found: Spanned<Token>,
    },

    #[error("expected one of the following '{expected:?}' but found {found}")]
    ExpectedOneTokens {
        expected: &'static [&'static TokenType],
        found: Spanned<Token>,
    },

    #[error("invalid type '{0:?}'")]
    InvalidType(Spanned<Token>),

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

fn parse_ident(tokens: &mut TokenStream) -> ParserResult<&String> {
    let tok = tokens.next();

    if let Token::Ident(ref ident) = tok.value {
        Ok(ident)
    } else {
        Err(ParserError::Unexpected(tok.clone()))
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

fn parse_type(tokens: &mut TokenStream) -> ParserResult<Type> {
    let tok = tokens.next();

    match tok.value {
        _ => Err(ParserError::InvalidType(tok.clone())),
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
        Token::Symbol(symbol @ Symbol::And)
        | Token::Symbol(symbol @ Symbol::Mul)
        | Token::Symbol(symbol @ Symbol::Not) => {
            tokens.consume();

            let expr = parse_unary(tokens)?;

            Ok(Expr::Unary(
                UnaryOp::from_symbol(&symbol).unwrap(),
                Box::new(expr),
            ))
        }
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

            let rhs = parse_additive(tokens)?;

            Ok(Expr::Binary(
                Box::new(lhs),
                BinOp::from_symbol(&symbol).unwrap(),
                Box::new(rhs),
            ))
        }
        _ => Ok(lhs),
    }
}

fn parse_comparative(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let lhs = parse_additive(tokens)?;
    let tok = tokens.peek();

    match tok.value {
        Token::Symbol(symbol @ Symbol::EqEq) => {
            tokens.consume();

            let rhs = parse_comparative(tokens)?;

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
    let lhs = parse_comparative(tokens)?;
    let tok = tokens.peek();

    match tok.value {
        Token::Symbol(Symbol::Eq) => {
            tokens.consume();

            let rhs = parse_assign(tokens)?;

            Ok(Expr::Assign(Box::new(lhs), Box::new(rhs)))
        }
        Token::Symbol(symbol @ Symbol::AddEq)
        | Token::Symbol(symbol @ Symbol::SubEq)
        | Token::Symbol(symbol @ Symbol::MulEq)
        | Token::Symbol(symbol @ Symbol::DivEq) => {
            tokens.consume();

            let rhs = parse_assign(tokens)?;

            Ok(Expr::Binary(
                Box::new(lhs),
                BinOp::from_symbol(&symbol).unwrap(),
                Box::new(rhs),
            ))
        }
        _ => Ok(lhs),
    }
}

pub fn parse_expr(tokens: &mut TokenStream) -> ParserResult<Expr> {
    parse_assign(tokens)
}

fn parse_let(tokens: &mut TokenStream) -> ParserResult<Expr> {
    expect(tokens, &Token::Keyword(Keyword::Let))?;

    let ident = Path::ident(parse_ident(tokens)?);

    expect(tokens, &Token::Symbol(Symbol::Eq))?;

    let value = parse_expr(tokens)?;

    expect(tokens, &Token::Symbol(Symbol::SemiColon))?;

    Ok(Expr::Let(ident, Box::new(value)))
}

fn parse_block_stmt(tokens: &mut TokenStream) -> ParserResult<Expr> {
    expect(tokens, &Token::Symbol(Symbol::Open(Delim::Brace)))?;

    let mut exprs = Vec::new();

    loop {
        let tok = tokens.peek();

        match tok.value {
            Token::Symbol(Symbol::Close(Delim::Brace)) => break,
            _ => exprs.push(parse_stmt(tokens)?),
        }
    }

    expect(tokens, &Token::Symbol(Symbol::Close(Delim::Brace)))?;

    Ok(Expr::Block(Block { exprs }))
}

fn parse_if_stmt(tokens: &mut TokenStream) -> ParserResult<Expr> {
    expect(tokens, &Token::Keyword(Keyword::If))?;

    let condition = parse_expr(tokens)?;

    let if_true = parse_block_stmt(tokens)?;

    let tok = tokens.peek();

    match tok.value {
        Token::Keyword(Keyword::Else) => {
            tokens.consume();

            let tok = tokens.peek();

            match tok.value {
                Token::Keyword(Keyword::If) => Ok(Expr::If(
                    Box::new(condition),
                    Box::new(if_true),
                    Some(Box::new(parse_if_stmt(tokens)?)),
                )),
                _ => Ok(Expr::If(
                    Box::new(condition),
                    Box::new(if_true),
                    Some(Box::new(parse_block_stmt(tokens)?)),
                )),
            }
        }
        _ => Ok(Expr::If(Box::new(condition), Box::new(if_true), None)),
    }
}

fn parse_while_stmt(tokens: &mut TokenStream) -> ParserResult<Expr> {
    expect(tokens, &Token::Keyword(Keyword::While))?;

    let condition = parse_expr(tokens)?;

    let block = parse_block_stmt(tokens)?;

    Ok(Expr::While(Box::new(condition), Box::new(block)))
}

pub fn parse_stmt(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let tok = tokens.peek();

    match tok.value {
        Token::Keyword(Keyword::Let) => parse_let(tokens),
        Token::Keyword(Keyword::If) => parse_if_stmt(tokens),
        Token::Keyword(Keyword::While) => parse_while_stmt(tokens),
        Token::Symbol(Symbol::Open(Delim::Brace)) => parse_block_stmt(tokens),
        Token::Symbol(Symbol::SemiColon) => {
            tokens.consume();

            Ok(Expr::Nop)
        }
        _ => {
            let expr = parse_expr(tokens)?;

            expect(tokens, &Token::Symbol(Symbol::SemiColon))?;

            Ok(expr)
        }
    }
}

pub fn parse_program(tokens: &mut TokenStream) -> ParserResult<Block> {
    let mut exprs = Vec::new();

    loop {
        if let Token::Eof = tokens.peek().value {
            break Ok(Block { exprs });
        } else {
            exprs.push(parse_stmt(tokens)?);
        }
    }
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
