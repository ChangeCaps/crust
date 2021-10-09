use super::{Instruction, Program, RuntimeResult, Stack};
use crate::compiler::Address;

pub struct Runtime {
    pub sp: Address,
    pub pc: usize,
    pub stack: Stack,
}

impl Runtime {
    pub fn new(size: usize) -> Self {
        Self {
            sp: Address(0),
            pc: 0,
            stack: Stack::new(size),
        }
    }

    #[inline]
    pub fn eval_instruction(&mut self, instruction: &Instruction) -> RuntimeResult<()> {
        match *instruction {
            Instruction::Init { dst, src, size } => {
                //println!("{} = {}", dst, self.stack.read_u32(src)?);
                self.stack.copy(dst, src, size)?;
            }
            Instruction::Push { dst, size } => {
                //println!("push({}) -> {} | {}", dst, self.sp, self.stack.read_u32(dst)?);
                self.stack.copy(self.sp, dst, size)?;
                self.sp.0 += size;
            }
            Instruction::Pop { dst } => {
                //println!("pop({}) | {} sp: {}", dst, self.stack.read_u32(self.sp)?, self.sp);
                self.sp.0 -= 4;
                self.stack.copy(dst, self.sp, 4)?;
            }
            Instruction::Mov { dst, src, size } => {
                //println!("{} = {} | {}", dst, src, self.stack.read_u32(src)?);
                self.stack.copy(dst, src, size)?;
            }
            Instruction::Ptr { dst, tgt } => {
                //println!("{} = &{} tgt: {} sp: {}", dst, Address((self.sp.0 as i64 + tgt) as u64), tgt, self.sp);
                self.stack.write_u32(dst, (self.sp.0 as i64 + tgt) as u32)?;
            }
            Instruction::Read { dst, ptr, size } => {
                //println!("{} = *{}", dst, ptr);
                let src = Address(self.stack.read_u32(ptr)? as u64);

                self.stack.copy(dst, src, size)?;
            }
            Instruction::Store { ptr, src, size } => {
                //println!("*{} = {} | {}", ptr, src, self.stack.read_u32(src)?);
                let dst = Address(self.stack.read_u32(ptr)? as u64);

                self.stack.copy(dst, src, size)?;
            }
            Instruction::ConstU32 { dst, src } => {
                //println!("{} = {}", dst, src);
                self.stack.write_u32(dst, src)?;
            }
            Instruction::AddI32 { dst, lhs, rhs } => {
                //println!("{} = {} + {}", dst, lhs, rhs);
                let res = self.stack.read_i32(lhs)? + self.stack.read_i32(rhs)?;

                self.stack.write_i32(dst, res)?;
            }
            Instruction::NegI32 { dst, tgt } => {
                //println!("{} = -{}", dst, tgt);
                let res = -self.stack.read_i32(tgt)?;

                self.stack.write_i32(dst, res)?;
            }
            Instruction::Eq {
                dst,
                lhs,
                rhs,
                size,
            } => {
                //println!("{} = {} == {}", dst, self.stack.read_u32(lhs)?, self.stack.read_u32(rhs)?);
                let res = self.stack.read(lhs, size)? == self.stack.read(rhs, size)?;

                self.stack.write_u32(dst, if res { 1 } else { 0 })?;
            }
            Instruction::CJmp { dst, tgt } => {
                //println!("jmpnz {} <- {}", dst, tgt);
                if self.stack.read_u32(tgt)? == 0 {
                    self.pc = dst;
                }
            }
            Instruction::Jmp { dst } => {
                //println!("jmp {}", dst);
                self.pc = dst;
            }
            Instruction::Call { func, ref args } => {
                self.stack.write_u32(Address(8), self.pc as u32)?;

                let func = self.stack.read_u32(func)? as usize;

                //println!("call");

                self.pc = func;

                for arg in args {
                    self.stack.copy(self.sp, *arg, 4)?;

                    self.sp.0 += 4;
                }
            }
            Instruction::Ret => {
                //println!("return");

                self.pc = self.stack.read_u32(Address(8))? as usize;
            }
        }

        Ok(())
    }

    #[inline]
    pub fn run_program(&mut self, program: &Program) -> RuntimeResult<()> {
        self.pc = 0;
        self.sp = program.stack_offset;

        self.stack.write(program.data_address, &program.data)?;

        while self.pc < program.instructions.len() {
            let instruction = &program.instructions[self.pc];

            self.pc += 1;
            self.eval_instruction(instruction)?;
        }

        Ok(())
    }
}
