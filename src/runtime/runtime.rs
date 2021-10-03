use crate::Address;

use super::{Instruction, Program, RuntimeResult, Stack};

pub struct Runtime {
    pub sp: Address,
    pub pc: usize,
    pub stack: Stack,
}

impl Runtime {
    pub fn new(size: usize) -> Self {
        Self {
            sp: 0,
            pc: 0,
            stack: Stack::new(size),
        }
    }

    #[inline]
    pub fn eval_instruction(&mut self, instruction: &Instruction) -> RuntimeResult<()> {
        match *instruction {
            Instruction::Init { dst, src, size } => {
                self.stack.copy(self.sp + dst, src, size)?;
            }
            Instruction::Mov { dst, src, size } => {
                self.stack.copy(self.sp + dst, self.sp + src, size)?;
            }
            Instruction::Ptr { dst, tgt } => {
                self.stack
                    .write_u32(self.sp + dst, (self.sp + tgt) as u32)?;
            }
            Instruction::Read { dst, ptr, size } => {
                let src = self.stack.read_u32(self.sp + ptr)? as Address;

                self.stack.copy(self.sp + dst, src, size)?;
            }
            Instruction::Store { ptr, src, size } => {
                let dst = self.stack.read_u32(self.sp + ptr)? as Address;

                self.stack.copy(dst, self.sp + src, size)?;
            }
            Instruction::AddI32 { dst, lhs, rhs } => {
                let res =
                    self.stack.read_i32(self.sp + lhs)? + self.stack.read_i32(self.sp + rhs)?;

                self.stack.write_i32(self.sp + dst, res)?;
            }
            Instruction::NegI32 { dst, tgt } => {
                let res = -self.stack.read_i32(self.sp + tgt)?;

                self.stack.write_i32(self.sp + dst, res)?;
            }
            Instruction::Eq {
                dst,
                lhs,
                rhs,
                size,
            } => {
                let res = self.stack.read(self.sp + lhs, size)?
                    == self.stack.read(self.sp + rhs, size)?;

                self.stack
                    .write_u32(self.sp + dst, if res { 1 } else { 0 })?;
            }
            Instruction::CJmp { dst, tgt } => {
                if self.stack.read_u32(self.sp + tgt)? == 0 {
                    self.pc = dst;
                }
            }
            Instruction::Jmp { dst } => {
                self.pc = dst;
            }
        }

        Ok(())
    }

    #[inline]
    pub fn run_program(&mut self, program: &Program) -> RuntimeResult<()> {
        self.pc = 0;
        self.sp = program.data.len();

        self.stack.write(0, &program.data)?;

        while self.pc < program.instructions.len() {
            let instruction = &program.instructions[self.pc];

            self.pc += 1;
            self.eval_instruction(instruction)?;
        }

        Ok(())
    }
}
