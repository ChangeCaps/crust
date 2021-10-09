use std::collections::HashMap;

use crate::{
    compiler::{mem_align, Abi, Address, Align, Compiler, EntryPoint, Register, Size, StackSlot},
    runtime::{Instruction, Program},
};

#[derive(Default)]
pub struct RuntimeCompiler {
    pub jumps: HashMap<EntryPoint, Vec<usize>>,
    pub entry_points: HashMap<EntryPoint, usize>,
    pub data: Vec<u8>,
    pub data_map: HashMap<Vec<u8>, Address>,
    pub instructions: Vec<Instruction>,
    pub stack: Address,
    pub stack_slots: HashMap<StackSlot, Address>,
}

impl RuntimeCompiler {
    pub const EAX: Address = Address(0);
    pub const EBX: Address = Address(4);
    pub const RET: Address = Address(8);

    pub const REGISTERS: u64 = 8;
    pub const DATA_OFFSET: u64 = 12 + Self::REGISTERS * 4;

    #[inline]
    pub fn register(&self, register: Register) -> Address {
        Address(register.0 * 4 + 12)
    }

    #[inline]
    pub fn push_data(&mut self, data: Vec<u8>) -> Address {
        if let Some(address) = self.data_map.get(&data) {
            *address
        } else {
            let address = Address(Self::DATA_OFFSET + self.data.len() as u64);

            self.data.append(&mut data.clone());

            self.data_map.insert(data, address);

            address
        }
    }
}

impl Compiler for RuntimeCompiler {
    type Output = Program;

    fn abi(&self) -> &Abi {
        &Abi { word: 4, align: 4 }
    }

    fn entry_point(&mut self, entry_point: EntryPoint) {
        let idx = self.instructions.len();

        if let Some(jumps) = self.jumps.get(&entry_point) {
            for jump in jumps {
                match self.instructions[*jump] {
                    Instruction::CJmp { ref mut dst, .. } => *dst = idx,
                    Instruction::Jmp { ref mut dst, .. } => *dst = idx,
                    Instruction::ConstU32 { ref mut src, .. } => *src = idx as u32,
                    _ => {}
                }
            }
        }

        if !entry_point.is_internal() {
            self.stack = Address(0);
            self.stack_slots.clear();
        }

        for argument in 0..entry_point.arg_count() {
            self.stack_slots.insert(StackSlot(argument), self.stack);

            self.stack.0 += 4;
        }

        self.entry_points.insert(entry_point, idx);
    }

    fn finish(&mut self) -> Self::Output {
        Program {
            stack_offset: Address(12 + Self::REGISTERS * 4 + self.data.len() as u64),
            data_address: Address(12 + Self::REGISTERS * 4),
            data: self.data.clone(),
            instructions: self.instructions.clone(),
        }
    }

    /* memory management */

    fn push(&mut self, src: Register, slot: Option<StackSlot>) {
        if let Some(slot) = slot {
            self.stack_slots.insert(slot, self.stack);
        }

        self.stack.0 += 4;

        self.instructions.push(Instruction::Push {
            dst: self.register(src),
            size: 4,
        });
    }

    fn pop(&mut self, dst: Option<Register>) {
        let register = match dst {
            Some(reg) => self.register(reg),
            None => Self::EAX,
        };

        self.stack.0 -= 4;

        self.instructions.push(Instruction::Pop { dst: register });
    }

    fn mov(&mut self, dst: Register, src: Register) {
        self.instructions.push(Instruction::Mov {
            dst: self.register(dst),
            src: self.register(src),
            size: 4,
        });
    }

    fn copy(&mut self, dst: Register, src: Register, size: Size) {
        self.instructions.push(Instruction::Mov {
            dst: self.register(dst),
            src: self.register(src),
            size,
        });
    }

    fn read(&mut self, dst: Register, src: Register) {
        self.instructions.push(Instruction::Read {
            dst: self.register(dst),
            ptr: self.register(src),
            size: 4,
        });
    }

    fn store(&mut self, dst: Register, src: Register) {
        self.instructions.push(Instruction::Store {
            ptr: self.register(dst),
            src: self.register(src),
            size: 4,
        });
    }

    fn stack_ptr(&mut self, dst: Register, src: StackSlot) {
        let offset = self.stack_slots[&src].0 as i64 - self.stack.0 as i64;

        self.instructions.push(Instruction::Ptr {
            dst: self.register(dst),
            tgt: offset,
        });
    }

    fn func_ptr(&mut self, dst: Register, src: EntryPoint) {
        if let Some(src) = self.entry_points.get(&src) {
            self.instructions.push(Instruction::ConstU32 {
                dst: self.register(dst),
                src: *src as u32,
            });
        } else {
            self.jumps
                .entry(src)
                .or_default()
                .push(self.instructions.len());

            self.instructions.push(Instruction::ConstU32 {
                dst: self.register(dst),
                src: 0,
            });
        }
    }

    /* boolean logic */
    fn not(&mut self, dst: Register, src: Register) {
        let data = self.push_data(i32::to_be_bytes(1).to_vec());
        self.instructions.push(Instruction::Init {
            dst: Self::EBX,
            src: data,
            size: 4,
        });
        self.instructions.push(Instruction::NegI32 {
            dst: Self::EAX,
            tgt: self.register(src),
        });
        self.instructions.push(Instruction::AddI32 {
            dst: self.register(dst),
            lhs: Self::EAX,
            rhs: Self::EBX,
        });
    }
    fn eq(&mut self, dst: Register, lhs: Register, rhs: Register) {
        self.instructions.push(Instruction::Eq {
            dst: self.register(dst),
            lhs: self.register(lhs),
            rhs: self.register(rhs),
            size: 4,
        });
    }

    /* i32 specific */

    fn const_i32(&mut self, dst: Register, src: i32) {
        let data = self.push_data(i32::to_be_bytes(src).to_vec());

        self.instructions.push(Instruction::Init {
            dst: self.register(dst),
            src: data,
            size: 4,
        });
    }

    fn neg_i32(&mut self, dst: Register, src: Register) {
        self.instructions.push(Instruction::NegI32 {
            dst: self.register(dst),
            tgt: self.register(src),
        });
    }

    fn add_i32(&mut self, dst: Register, lhs: Register, rhs: Register) {
        self.instructions.push(Instruction::AddI32 {
            dst: self.register(dst),
            lhs: self.register(lhs),
            rhs: self.register(rhs),
        });
    }

    fn sub_i32(&mut self, dst: Register, lhs: Register, rhs: Register) {
        self.instructions.push(Instruction::NegI32 {
            dst: Self::EAX,
            tgt: self.register(rhs),
        });

        self.instructions.push(Instruction::AddI32 {
            dst: self.register(dst),
            lhs: self.register(lhs),
            rhs: Self::EAX,
        });
    }

    /* control flow */

    fn jmp(&mut self, dst: EntryPoint) {
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

    fn jmp_nz(&mut self, dst: EntryPoint, src: Register) {
        if let Some(dst) = self.entry_points.get(&dst) {
            self.instructions.push(Instruction::CJmp {
                dst: *dst,
                tgt: self.register(src),
            });
        } else {
            self.jumps
                .entry(dst)
                .or_default()
                .push(self.instructions.len());

            self.instructions.push(Instruction::CJmp {
                dst: 0,
                tgt: self.register(src),
            });
        }
    }

    fn call(&mut self, dst: Register, func: Register, args: Vec<Register>) {
        self.instructions.push(Instruction::Push {
            dst: Self::RET,
            size: 4,
        });

        self.instructions.push(Instruction::Call {
            func: self.register(func),
            args: args.into_iter().map(|reg| self.register(reg)).collect(),
        });

        self.instructions.push(Instruction::Pop { dst: Self::RET });

        self.instructions.push(Instruction::Mov {
            dst: self.register(dst),
            src: Self::EAX,
            size: 4,
        });
    }

    fn ret(&mut self, src: Register) {
        self.instructions.push(Instruction::Mov {
            dst: Self::EAX,
            src: self.register(src),
            size: 4,
        });

        self.instructions.push(Instruction::Ret);
    }

    fn exit(&mut self, _src: Register) {
        self.instructions.push(Instruction::Jmp { dst: usize::MAX });
    }
}
