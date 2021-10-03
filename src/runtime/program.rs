use super::Instruction;

pub struct Program {
    pub data: Vec<u8>,
    pub instructions: Vec<Instruction>,
}
