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
    Neg,
    Not,
}

impl UnaryOp {
    #[inline]
    pub fn from_symbol(symbol: &Symbol) -> Option<Self> {
        Some(match symbol {
            Symbol::And => Self::Ref,
            Symbol::Mul => Self::Deref,
            Symbol::Sub => Self::Neg,
            Symbol::Not => Self::Not,
            _ => return None,
        })
    }
}

impl std::fmt::Display for UnaryOp {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref => write!(f, "&"),
            Self::Deref => write!(f, "*"),
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    Eq,
}

impl BinOp {
    #[inline]
    pub fn from_symbol(symbol: &Symbol) -> Option<Self> {
        Some(match symbol {
            Symbol::Add => Self::Add,
            Symbol::Sub => Self::Sub,
            Symbol::Mul => Self::Mul,
            Symbol::Div => Self::Div,
            Symbol::AddEq => Self::AddEq,
            Symbol::SubEq => Self::SubEq,
            Symbol::MulEq => Self::MulEq,
            Symbol::DivEq => Self::DivEq,
            Symbol::EqEq => Self::Eq,
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
            Self::AddEq => write!(f, "+="),
            Self::SubEq => write!(f, "-="),
            Self::MulEq => write!(f, "*="),
            Self::DivEq => write!(f, "/="),
            Self::Eq => write!(f, "=="),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Nop,
    Literal(Literal),
    Paren(Box<Expr>),
    Path(Path),
    Let(Path, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Block(Block),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
}

impl std::fmt::Display for Expr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nop => write!(f, ""),
            Self::Literal(lit) => lit.fmt(f),
            Self::Paren(expr) => write!(f, "({})", expr),
            Self::Let(path, expr) => write!(f, "let {} = {};\n", path, expr),
            Self::Path(path) => path.fmt(f),
            Self::Assign(lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            Self::Unary(op, expr) => write!(f, "{}{}", op, expr),
            Self::Binary(lhs, op, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            Self::Block(block) => {
                write!(f, "{{\n")?;

                for expr in &block.exprs {
                    write!(f, "{}", expr)?;
                }

                write!(f, "}}\n")?;

                Ok(())
            }
            Self::If(condition, if_true, if_false) => {
                if let Some(if_false) = if_false {
                    write!(f, "if {} {} else {}", condition, if_true, if_false)
                } else {
                    write!(f, "if {} {}", condition, if_true)
                }
            }
            Self::While(condition, block) => {
                write!(f, "while {} {{\n {} \n}}\n", condition, block)
            }
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub exprs: Vec<Expr>,
}
