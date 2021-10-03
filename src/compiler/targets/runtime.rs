use std::collections::HashMap;

use crate::{
    compiler::{Compiler, EntryPoint},
    runtime::{Instruction, Program},
    Address, Size,
};

#[derive(Default)]
pub struct RuntimeCompiler {
    pub jumps: HashMap<EntryPoint, Vec<usize>>,
    pub entry_points: HashMap<EntryPoint, usize>,
    pub data: Vec<u8>,
    pub data_map: HashMap<Vec<u8>, usize>,
    pub instructions: Vec<Instruction>,
}

impl RuntimeCompiler {
    #[inline]
    pub fn push_data(&mut self, data: Vec<u8>) -> usize {
        if let Some(address) = self.data_map.get(&data) {
            *address
        } else {
            let address = self.data.len();

            self.data.append(&mut data.clone());

            self.data_map.insert(data, address);

            address
        }
    }
}

impl Compiler for RuntimeCompiler {
    type Output = Program;

    fn entry_point(&mut self, entry_point: EntryPoint) {
        let idx = self.instructions.len();

        if let Some(jumps) = self.jumps.remove(&entry_point) {
            for instruction in jumps {
                match self.instructions[instruction] {
                    Instruction::CJmp { ref mut dst, .. } => {
                        *dst = idx;
                    }
                    Instruction::Jmp { ref mut dst, .. } => {
                        *dst = idx;
                    }
                    _ => {}
                }
            }
        }

        self.entry_points.insert(entry_point, idx);
    }

    fn finish(&mut self) -> Self::Output {
        Program {
            data: self.data.clone(),
            instructions: self.instructions.clone(),
        }
    }

    fn init(&mut self, target: Address, value: Vec<u8>) {
        let size = value.len();
        let address = self.push_data(value);

        self.instructions.push(Instruction::Init {
            dst: target,
            src: address,
            size,
        })
    }

    fn add_i32(&mut self, dst: Address, lhs: Address, rhs: Address) {
        self.instructions
            .push(Instruction::AddI32 { dst, lhs, rhs });
    }

    fn neg_i32(&mut self, dst: Address, value: Address) {
        self.instructions
            .push(Instruction::NegI32 { dst, tgt: value });
    }

    fn copy(&mut self, dst: Address, src: Address, size: Size) {
        self.instructions.push(Instruction::Mov { dst, src, size });
    }

    fn address_of(&mut self, dst: Address, tgt: Address) {
        self.instructions.push(Instruction::Ptr { dst, tgt });
    }

    fn read(&mut self, dst: Address, src: Address, size: Size) {
        self.instructions.push(Instruction::Read {
            dst,
            ptr: src,
            size,
        });
    }

    fn write(&mut self, dst: Address, src: Address, size: Size) {
        self.instructions.push(Instruction::Store {
            ptr: dst,
            src,
            size,
        });
    }

    fn eq(&mut self, dst: Address, lhs: Address, rhs: Address, size: Size) {
        self.instructions.push(Instruction::Eq {
            dst,
            lhs,
            rhs,
            size,
        });
    }

    fn not(&mut self, dst: Address, tgt: Address) {
        let one = self.push_data(u32::to_be_bytes(1).to_vec());

        self.instructions.push(Instruction::NegI32 { dst, tgt });
        self.instructions.push(Instruction::Init {
            dst: dst + 4,
            src: one,
            size: 4,
        });
        self.instructions.push(Instruction::AddI32 {
            dst,
            lhs: dst,
            rhs: dst + 4,
        });
    }

    fn conditional_jump(&mut self, dst: EntryPoint, condition: Address) {
        if let Some(dst) = self.entry_points.get(&dst) {
            self.instructions.push(Instruction::CJmp {
                dst: *dst,
                tgt: condition,
            });
        } else {
            self.jumps
                .entry(dst)
                .or_default()
                .push(self.instructions.len());

            self.instructions.push(Instruction::CJmp {
                dst: 0,
                tgt: condition,
            });
        }
    }

    fn jump(&mut self, dst: EntryPoint) {
        if let Some(dst) = self.entry_points.get(&dst) {
            self.instructions.push(Instruction::Jmp { dst: *dst });
        } else {
            self.jumps
                .entry(dst)
                .or_default()
                .push(self.instructions.len());

            self.instructions.push(Instruction::Jmp { dst: 0 });
        }
    }

    fn call(&mut self, func: Address, function: EntryPoint, parameters: &[Address]) {
        todo!()
    }

    fn ret(&mut self, dst: Address) {
        todo!()
    }
}
