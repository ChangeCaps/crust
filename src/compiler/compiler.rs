use super::{Align, EntryPoint, Register, Size, StackSlot};

pub struct Abi {
    pub word: u64,
    pub align: u64,
}

pub trait Compiler {
    type Output;

    fn abi(&self) -> &Abi;

    fn entry_point(&mut self, entry_point: EntryPoint);

    fn finish(&mut self) -> Self::Output;

    /* memory */

    fn push(&mut self, src: Register, slot: Option<StackSlot>);
    fn pop(&mut self, dst: Option<Register>);
    fn mov(&mut self, dst: Register, src: Register);
    fn copy(&mut self, dst: Register, src: Register, size: Size);
    fn read(&mut self, dst: Register, src: Register);
    fn store(&mut self, dst: Register, src: Register);
    fn stack_ptr(&mut self, dst: Register, src: StackSlot);
    fn func_ptr(&mut self, dst: Register, src: EntryPoint);
    fn zero(&mut self, dst: Register) {
        self.const_i32(dst, 0);
    }

    /* boolean logic */

    fn const_bool(&mut self, dst: Register, src: bool) {
        self.const_i32(dst, if src { 1 } else { 0 });
    }
    fn not(&mut self, dst: Register, src: Register);
    fn eq(&mut self, dst: Register, lhs: Register, rhs: Register);
    fn not_eq(&mut self, dst: Register, lhs: Register, rhs: Register) {
        self.eq(dst, lhs, rhs);
        self.not(dst, dst);
    }

    /* i32 */

    fn const_i32(&mut self, dst: Register, src: i32);
    fn neg_i32(&mut self, dst: Register, src: Register);
    fn add_i32(&mut self, dst: Register, lhs: Register, rhs: Register);
    fn sub_i32(&mut self, dst: Register, lhs: Register, rhs: Register);

    /* control flow */

    fn jmp(&mut self, dst: EntryPoint);
    fn jmp_nz(&mut self, dst: EntryPoint, src: Register);
    fn call(&mut self, dst: Register, func: Register, args: Vec<Register>);
    fn ret(&mut self, src: Register);
    fn exit(&mut self, src: Register);
}
