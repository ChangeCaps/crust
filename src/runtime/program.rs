use crate::compiler::Address;

use super::Instruction;

pub struct Program {
    pub stack_offset: Address,
    pub data_address: Address,
    pub data: Vec<u8>,
    pub instructions: Vec<Instruction>,
}
