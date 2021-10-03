use crate::{compiler::EntryPoint, Address, Size};

pub type InstructionIndex = usize;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Init {
        dst: Address,
        src: Address,
        size: Size,
    },
    Mov {
        dst: Address,
        src: Address,
        size: Size,
    },
    Ptr {
        dst: Address,
        tgt: Address,
    },
    Read {
        dst: Address,
        ptr: Address,
        size: Size,
    },
    Store {
        ptr: Address,
        src: Address,
        size: Size,
    },
    AddI32 {
        dst: Address,
        lhs: Address,
        rhs: Address,
    },
    NegI32 {
        dst: Address,
        tgt: Address,
    },
    Eq {
        dst: Address,
        lhs: Address,
        rhs: Address,
        size: Size,
    },
    CJmp {
        dst: InstructionIndex,
        tgt: Address,
    },
    Jmp {
        dst: InstructionIndex,
    },
}
