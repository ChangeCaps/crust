mod compile_ir;
pub mod compiler;
mod module;
mod statement;
pub mod targets;
mod type_registry;
mod value;

pub use compile_ir::*;
pub use compiler::*;
pub use module::*;
pub use statement::*;
pub use type_registry::*;
pub use value::*;

pub fn mem_align(size: Size, align: Align) -> Size {
    if size == 0 {
        0
    } else {
        (size + align - 1) / size * align
    }
}

pub type Size = u64;
pub type Align = u64;

#[repr(transparent)]
#[derive(Clone, Copy, Default, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Register(pub u64);

impl std::fmt::Display for Register {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r%{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StackSlot(pub u64);

impl std::fmt::Display for StackSlot {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "s%{}", self.0)
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Default, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Address(pub u64);

impl std::fmt::Display for Address {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m%{}", self.0)
    }
}

impl std::ops::Add<Address> for Address {
    type Output = Self;

    #[inline]
    fn add(self, other: Self) -> Self::Output {
        Address(self.0 + other.0)
    }
}
