use super::Register;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    F32,
    Str,
    Bool,
    Ref(Box<Type>),
    Func(Vec<Type>, Box<Type>),
    Void,
}

impl std::fmt::Display for Type {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "i32"),
            Self::F32 => write!(f, "f32"),
            Self::Str => write!(f, "str"),
            Self::Bool => write!(f, "bool"),
            Self::Ref(ty) => write!(f, "*{}", ty),
            Self::Func(args, return_type) => {
                write!(f, "fn(")?;

                for (i, arg) in args.iter().enumerate() {
                    if i != args.len() - 1 {
                        write!(f, "{}, ", arg)?;
                    } else {
                        write!(f, "{}", arg)?;
                    }
                }

                write!(f, ") -> {}", return_type)
            }
            Self::Void => write!(f, "void"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Value {
    pub ty: Type,
    pub register: Register,
    pub implicit_dereference: bool,
}

impl Value {
    #[inline]
    pub fn unused() -> Self {
        Self {
            ty: Type::Void,
            register: Register(0),
            implicit_dereference: false,
        }
    }
}
