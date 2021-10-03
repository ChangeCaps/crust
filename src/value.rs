use crate::{Address, Size};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    F32,
    Str,
    Bool,
    Ref(Box<Type>),
    Unit,
}

impl Type {
    #[inline]
    pub fn size(&self) -> Option<Size> {
        Some(match self {
            Self::I32 => 4,
            Self::F32 => 4,
            Self::Bool => 4,
            Self::Ref(_) => 4,
            Self::Unit => 0,
            _ => return None,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Value {
    pub ty: Type,
    pub address: Address,
    pub stored: bool,
    pub ptr: bool,
}

impl Value {
    #[inline]
    pub fn new(ty: Type, address: Address, stored: bool, ptr: bool) -> Self {
        Self {
            ty,
            address,
            stored,
            ptr,
        }
    }

    #[inline]
    pub fn stored(ty: Type, address: Address) -> Self {
        Self {
            ty,
            address,
            stored: true,
            ptr: false,
        }
    }

    #[inline]
    pub fn temp(ty: Type, address: Address) -> Self {
        Self {
            ty,
            address,
            stored: false,
            ptr: false,
        }
    }
}
