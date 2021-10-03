use crate::{Address, Size};

use super::{RuntimeError, RuntimeResult};

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
    pub fn copy(&mut self, dst: Address, src: Address, size: Size) -> RuntimeResult<()> {
        let upper_bound = Address::max(src, dst) + size;

        if upper_bound > self.data.len() {
            if self.allow_resize {
                self.data.resize(upper_bound, 0);
            } else {
                return Err(RuntimeError::OutOfBounds);
            }
        }

        self.data.copy_within(src..src + size, dst);

        Ok(())
    }

    #[inline]
    pub fn write(&mut self, address: Address, data: &[u8]) -> RuntimeResult<()> {
        let size = data.len();

        let upper_bound = address + size;

        if upper_bound > self.data.len() {
            if self.allow_resize {
                self.data.resize(upper_bound, 0);
            } else {
                return Err(RuntimeError::OutOfBounds);
            }
        }

        self.data[address..upper_bound].copy_from_slice(data);

        Ok(())
    }

    #[inline]
    pub fn read(&self, address: Address, size: Size) -> RuntimeResult<&[u8]> {
        let upper_bound = address + size;

        if upper_bound <= self.data.len() {
            Ok(&self.data[address..upper_bound])
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
}

impl std::fmt::Display for Stack {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, chunk) in self.data.chunks(4).enumerate() {
            write!(f, "{:04}:  ", i * 4)?;

            for byte in chunk {
                write!(f, "{:02X}", byte)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}
