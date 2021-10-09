use std::ops::Range;

use super::{RuntimeError, RuntimeResult};
use crate::compiler::{Address, Size};

pub struct Stack {
    data: Vec<u8>,
    allow_resize: bool,
}

impl Stack {
    #[inline]
    pub fn new(size: usize) -> Self {
        Self {
            data: vec![0; size],
            allow_resize: false,
        }
    }

    #[inline]
    pub fn resizable() -> Self {
        Self {
            data: Vec::new(),
            allow_resize: true,
        }
    }

    #[inline]
    pub fn end(&self) -> Address {
        Address(self.data.len() as u64)
    }

    #[inline]
    pub fn copy(&mut self, dst: Address, src: Address, size: Size) -> RuntimeResult<()> {
        let upper_bound = u64::max(src.0, dst.0) + size;

        if upper_bound > self.data.len() as u64 {
            if self.allow_resize {
                self.data.resize(upper_bound as usize, 0);
            } else {
                return Err(RuntimeError::OutOfBounds);
            }
        }

        self.data
            .copy_within(src.0 as usize..(src.0 + size) as usize, dst.0 as usize);

        Ok(())
    }

    #[inline]
    pub fn write(&mut self, address: Address, data: &[u8]) -> RuntimeResult<()> {
        let size = data.len();

        let upper_bound = address.0 + size as u64;

        if upper_bound > self.data.len() as u64 {
            if self.allow_resize {
                self.data.resize(upper_bound as usize, 0);
            } else {
                return Err(RuntimeError::OutOfBounds);
            }
        }

        self.data[address.0 as usize..upper_bound as usize].copy_from_slice(data);

        Ok(())
    }

    #[inline]
    pub fn read(&self, address: Address, size: Size) -> RuntimeResult<&[u8]> {
        let upper_bound = address.0 + size;

        if upper_bound <= self.data.len() as u64 {
            Ok(&self.data[address.0 as usize..upper_bound as usize])
        } else {
            Err(RuntimeError::OutOfBounds)
        }
    }

    #[inline]
    pub fn read_i32(&self, address: Address) -> RuntimeResult<i32> {
        let int = self.read(address, 4)?;
        Ok(i32::from_be_bytes([int[0], int[1], int[2], int[3]]))
    }

    #[inline]
    pub fn read_u32(&self, address: Address) -> RuntimeResult<u32> {
        let int = self.read(address, 4)?;
        Ok(u32::from_be_bytes([int[0], int[1], int[2], int[3]]))
    }

    #[inline]
    pub fn write_i32(&mut self, address: Address, int: i32) -> RuntimeResult<()> {
        self.write(address, &i32::to_be_bytes(int))
    }

    #[inline]
    pub fn write_u32(&mut self, address: Address, int: u32) -> RuntimeResult<()> {
        self.write(address, &u32::to_be_bytes(int))
    }

    #[inline]
    pub fn into_data(self) -> Vec<u8> {
        self.data
    }

    #[inline]
    pub fn display_range(&self, range: Range<Address>) -> DisplayStack<'_> {
        DisplayStack { stack: self, range }
    }
}

impl std::fmt::Display for Stack {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, chunk) in self.data.chunks(4).enumerate() {
            write!(f, "{:08X}: ", i * 4)?;

            for byte in chunk {
                write!(f, " {:02X}", byte)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

pub struct DisplayStack<'a> {
    stack: &'a Stack,
    range: Range<Address>,
}

impl std::fmt::Display for DisplayStack<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, chunk) in self.stack.data[self.range.start.0 as usize..self.range.end.0 as usize]
            .chunks(4)
            .enumerate()
        {
            write!(f, "{:08X}: ", i * 4 + self.range.start.0 as usize)?;

            for byte in chunk {
                write!(f, " {:02X}", byte)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}
