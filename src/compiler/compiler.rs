use crate::{Address, Size};

use super::EntryPoint;

pub trait Compiler {
    type Output;

    fn entry_point(&mut self, entry_point: EntryPoint);

    fn finish(&mut self) -> Self::Output;

    fn init(&mut self, target: Address, value: Vec<u8>);
    fn add_i32(&mut self, dst: Address, lhs: Address, rhs: Address);
    fn neg_i32(&mut self, dst: Address, value: Address);
    fn copy(&mut self, dst: Address, src: Address, size: Size);
    fn address_of(&mut self, dst: Address, tgt: Address);
    fn read(&mut self, dst: Address, src: Address, size: Size);
    fn write(&mut self, dst: Address, src: Address, size: Size);
    fn eq(&mut self, dst: Address, lhs: Address, rhs: Address, size: Size);
    fn not(&mut self, dst: Address, tgt: Address);
    fn conditional_jump(&mut self, dst: EntryPoint, condition: Address);
    fn jump(&mut self, dst: EntryPoint);
    fn call(&mut self, func: Address, function: EntryPoint, parameters: &[Address]);
    fn ret(&mut self, dst: Address);
}
