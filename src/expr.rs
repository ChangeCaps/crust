use crate::{
    path::Path,
    token::{IntegerFormat, Symbol},
};

#[derive(Debug)]
pub enum Literal {
    Integer(i64, IntegerFormat),
}

impl std::fmt::Display for Literal {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(int, format) => match format {
                IntegerFormat::Binary => write!(f, "0b{:b}", int),
                IntegerFormat::Decimal => write!(f, "{}", int),
                IntegerFormat::Hexadecimal => {
                    if *int < 0 {
                        write!(f, "-0x{:X}", int.abs())
                    } else {
                        write!(f, "0x{:X}", int.abs())
                    }
                }
            },
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Ref,
    Deref,
}

impl std::fmt::Display for UnaryOp {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref => write!(f, "&"),
            Self::Deref => write!(f, "*"),
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOp {
    #[inline]
    pub fn from_symbol(symbol: &Symbol) -> Option<Self> {
        Some(match symbol {
            Symbol::Add => Self::Add,
            Symbol::Sub => Self::Sub,
            Symbol::Mul => Self::Mul,
            Symbol::Div => Self::Div,
            _ => return None,
        })
    }
}

impl std::fmt::Display for BinOp {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Paren(Box<Expr>),
    Path(Path),
    Assign(Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
}

impl std::fmt::Display for Expr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(lit) => lit.fmt(f),
            Self::Paren(expr) => write!(f, "({})", expr),
            Self::Path(path) => path.fmt(f),
            Self::Assign(lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            Self::Unary(op, expr) => write!(f, "{}{}", op, expr),
            Self::Binary(lhs, op, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
        }
    }
}
