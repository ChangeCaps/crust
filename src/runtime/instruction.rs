use crate::compiler::{Address, Size};

pub type InstructionIndex = usize;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Init {
        dst: Address,
        src: Address,
        size: Size,
    },
    Push {
        dst: Address,
        size: Size,
    },
    Pop {
        dst: Address,
    },
    Mov {
        dst: Address,
        src: Address,
        size: Size,
    },
    Ptr {
        dst: Address,
        tgt: i64,
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
    ConstU32 {
        dst: Address,
        src: u32,
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
    Call {
        func: Address,
        args: Vec<Address>,
    },
    Ret,
}
