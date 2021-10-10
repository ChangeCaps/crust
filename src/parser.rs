use thiserror::Error;

use crate::{
    compiler::Type,
    decl::{Decl, FunctionArg, FunctionDecl, ModuleDecl},
    expr::{BinOp, Block, DeclOrExpr, Expr, Literal, UnaryOp},
    path::{Path, PathSegment},
    span::Spanned,
    token::{Delim, Keyword, Symbol, Token, TokenType},
    token_stream::TokenStream,
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
    let tok = tokens.next();

    if tok.value == *token {
        Ok(())
    } else {
        Err(ParserError::MissingExpectedToken {
            expected: token.ty(),
            found: tok.clone(),
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
        Token::Keyword(Keyword::Super) => Ok(PathSegment::Super),
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
        Token::Keyword(Keyword::I32) => Ok(Type::I32),
        Token::Keyword(Keyword::F32) => Ok(Type::F32),
        Token::Keyword(Keyword::Bool) => Ok(Type::Bool),
        Token::Keyword(Keyword::Void) => Ok(Type::Void),
        Token::Keyword(Keyword::Str) => Ok(Type::Str),
        Token::Keyword(Keyword::Fn) => {
            expect(tokens, &Token::Symbol(Symbol::Open(Delim::Paren)))?;

            let mut args = Vec::new();

            loop {
                if let Token::Symbol(Symbol::Close(Delim::Paren)) = tokens.peek().value {
                    tokens.consume();

                    break;
                }

                if !args.is_empty() {
                    expect(tokens, &Token::Symbol(Symbol::Comma))?;
                }

                args.push(parse_type(tokens)?);
            }

            let return_type = if let Token::Symbol(Symbol::Arrow) = tokens.peek().value {
                tokens.consume();

                parse_type(tokens)?
            } else {
                Type::Void
            };

            Ok(Type::Func(args, Box::new(return_type)))
        }
        Token::Symbol(Symbol::Mul) => Ok(Type::Ref(Box::new(parse_type(tokens)?))),
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
        Token::Keyword(Keyword::True) => {
            tokens.consume();

            Ok(Expr::Literal(Literal::Bool(true)))
        }
        Token::Keyword(Keyword::False) => {
            tokens.consume();

            Ok(Expr::Literal(Literal::Bool(false)))
        }
        Token::Keyword(Keyword::Void) => {
            tokens.consume();

            Ok(Expr::Literal(Literal::Void))
        }
        Token::Ident(_) | Token::Keyword(Keyword::Super) => {
            let path = parse_path(tokens)?;

            Ok(Expr::Path(path))
        }
        Token::Symbol(Symbol::Open(Delim::Paren)) => {
            tokens.consume();

            let expr = parse_expr(tokens)?;

            expect(tokens, &Token::Symbol(Symbol::Close(Delim::Paren)))?;

            Ok(Expr::Paren(Box::new(expr)))
        }
        Token::Symbol(Symbol::Lt) => {
            tokens.consume();

            let ty = parse_type(tokens)?;

            expect(tokens, &Token::Symbol(Symbol::Gt))?;
            expect(tokens, &Token::Symbol(Symbol::Open(Delim::Paren)))?;

            let val = parse_expr(tokens)?;

            expect(tokens, &Token::Symbol(Symbol::Close(Delim::Paren)))?;

            Ok(Expr::Cast(Box::new(val), ty))
        }
        _ => Err(ParserError::Unexpected(tok.clone())),
    }
}

fn parse_function_params(tokens: &mut TokenStream) -> ParserResult<Vec<Expr>> {
    let mut params = Vec::new();

    loop {
        if !params.is_empty() {
            expect(tokens, &Token::Symbol(Symbol::Comma))?;
        }

        let tok = tokens.peek();

        match tok.value {
            Token::Symbol(Symbol::Close(Delim::Paren)) => {
                tokens.consume();

                break Ok(params);
            }
            _ => {
                params.push(parse_expr(tokens)?);

                if let Token::Symbol(Symbol::Close(Delim::Paren)) = tokens.peek().value {
                    tokens.consume();

                    break Ok(params);
                }
            }
        }
    }
}

fn parse_call(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let term = parse_term(tokens)?;
    let tok = tokens.peek();

    match tok.value {
        Token::Symbol(Symbol::Open(Delim::Paren)) => {
            tokens.consume();

            let params = parse_function_params(tokens)?;

            Ok(Expr::Call(Box::new(term), params))
        }
        _ => Ok(term),
    }
}

fn parse_unary(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let tok = tokens.peek();

    match tok.value {
        Token::Symbol(symbol @ Symbol::And)
        | Token::Symbol(symbol @ Symbol::Mul)
        | Token::Symbol(symbol @ Symbol::Sub)
        | Token::Symbol(symbol @ Symbol::Not) => {
            tokens.consume();

            let expr = parse_unary(tokens)?;

            Ok(Expr::Unary(
                UnaryOp::from_symbol(&symbol).unwrap(),
                Box::new(expr),
            ))
        }
        _ => parse_call(tokens),
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
        Token::Symbol(symbol @ Symbol::EqEq) | Token::Symbol(symbol @ Symbol::NotEq) => {
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
    let tok = tokens.peek();

    match tok.value {
        Token::Comment(ref comment) => {
            let expr = Expr::Comment(comment.clone());

            tokens.consume();

            Ok(expr)
        }
        _ => parse_assign(tokens),
    }
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

    let mut decls = Vec::new();
    let mut exprs = Vec::new();

    loop {
        let tok = tokens.peek();

        match tok.value {
            Token::Symbol(Symbol::Close(Delim::Brace)) => break,
            _ => match parse_decl_or_expr(tokens)? {
                DeclOrExpr::Decl(decl) => decls.push(decl),
                DeclOrExpr::Expr(expr) => exprs.push(expr),
            },
        }
    }

    expect(tokens, &Token::Symbol(Symbol::Close(Delim::Brace)))?;

    Ok(Expr::Block(Block { decls, exprs }))
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

fn parse_return_stmt(tokens: &mut TokenStream) -> ParserResult<Expr> {
    expect(tokens, &Token::Keyword(Keyword::Return))?;

    let val = parse_expr(tokens)?;

    expect(tokens, &Token::Symbol(Symbol::SemiColon))?;

    Ok(Expr::Return(Box::new(val)))
}

pub fn parse_stmt(tokens: &mut TokenStream) -> ParserResult<Expr> {
    let tok = tokens.peek();

    match tok.value {
        Token::Comment(ref comment) => {
            let expr = Expr::Comment(comment.clone());

            tokens.consume();

            Ok(expr)
        }
        Token::Keyword(Keyword::Let) => parse_let(tokens),
        Token::Keyword(Keyword::If) => parse_if_stmt(tokens),
        Token::Keyword(Keyword::While) => parse_while_stmt(tokens),
        Token::Keyword(Keyword::Return) => parse_return_stmt(tokens),
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

fn parse_function_args(tokens: &mut TokenStream) -> ParserResult<Vec<FunctionArg>> {
    expect(tokens, &Token::Symbol(Symbol::Open(Delim::Paren)))?;

    let mut args = Vec::new();

    loop {
        let tok = tokens.next();

        match tok.value {
            Token::Ident(ref ident) => {
                let ident = ident.clone();

                expect(tokens, &Token::Symbol(Symbol::Colon))?;

                let ty = parse_type(tokens)?;

                args.push(FunctionArg { ident, ty });

                let tok = tokens.next();

                match tok.value {
                    Token::Symbol(Symbol::Comma) => {}
                    Token::Symbol(Symbol::Close(Delim::Paren)) => break Ok(args),
                    _ => {
                        break Err(ParserError::ExpectedOneTokens {
                            expected: &[
                                &TokenType::SpecificSymbol(Symbol::Comma),
                                &TokenType::SpecificSymbol(Symbol::Close(Delim::Paren)),
                            ],
                            found: tok.clone(),
                        })
                    }
                }
            }
            Token::Symbol(Symbol::Close(Delim::Paren)) => break Ok(args),
            _ => {
                break Err(ParserError::ExpectedOneTokens {
                    expected: &[
                        &TokenType::SpecificSymbol(Symbol::Close(Delim::Paren)),
                        &TokenType::Ident,
                    ],
                    found: tok.clone(),
                })
            }
        }
    }
}

fn parse_function_decl(tokens: &mut TokenStream) -> ParserResult<Decl> {
    expect(tokens, &Token::Keyword(Keyword::Fn))?;

    let ident = parse_ident(tokens)?.clone();

    let args = parse_function_args(tokens)?;

    let tok = tokens.peek();

    let return_type = if let Token::Symbol(Symbol::Arrow) = tok.value {
        tokens.consume();

        parse_type(tokens)?
    } else {
        Type::Void
    };

    let expr = parse_block_stmt(tokens)?;

    Ok(Decl::Function(FunctionDecl {
        ident,
        return_type,
        args,
        expr,
    }))
}

fn parse_module_decl(tokens: &mut TokenStream) -> ParserResult<Decl> {
    expect(tokens, &Token::Keyword(Keyword::Mod))?;

    let ident = parse_ident(tokens)?.clone();

    expect(tokens, &Token::Symbol(Symbol::Open(Delim::Brace)))?;

    let mut decls = Vec::new();

    loop {
        if let Token::Symbol(Symbol::Close(Delim::Brace)) = tokens.peek().value {
            tokens.consume();

            break Ok(Decl::Module(ModuleDecl { name: ident, decls }));
        }

        decls.push(parse_decl(tokens)?);
    }
}

pub fn parse_decl(tokens: &mut TokenStream) -> ParserResult<Decl> {
    let tok = tokens.peek();

    match tok.value {
        Token::Keyword(Keyword::Fn) => parse_function_decl(tokens),
        Token::Keyword(Keyword::Mod) => parse_module_decl(tokens),
        Token::Keyword(Keyword::Use) => {
            tokens.consume();

            let path = parse_path(tokens)?;

            expect(tokens, &Token::Symbol(Symbol::SemiColon))?;

            Ok(Decl::Use(path))
        }
        _ => Err(ParserError::Unexpected(tok.clone())),
    }
}

pub fn parse_decl_or_expr(tokens: &mut TokenStream) -> ParserResult<DeclOrExpr> {
    let tok = tokens.peek();

    match tok.value {
        Token::Keyword(Keyword::Fn)
        | Token::Keyword(Keyword::Mod)
        | Token::Keyword(Keyword::Use) => Ok(DeclOrExpr::Decl(parse_decl(tokens)?)),
        _ => Ok(DeclOrExpr::Expr(parse_stmt(tokens)?)),
    }
}

pub fn parse_program(tokens: &mut TokenStream) -> ParserResult<Block> {
    let mut decls = Vec::new();
    let mut exprs = Vec::new();

    loop {
        if let Token::Eof = tokens.peek().value {
            break Ok(Block { decls, exprs });
        } else {
            match parse_decl_or_expr(tokens)? {
                DeclOrExpr::Decl(decl) => decls.push(decl),
                DeclOrExpr::Expr(expr) => exprs.push(expr),
            }
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
