use super::EntryPoint;
use crate::{Address, Size};

#[derive(Clone, Debug)]
pub enum Statement {
    /// Does nothing.
    Nop,
    /// Initializes ``target`` with bytes of ``value``
    Init { target: Address, value: Vec<u8> },
    /// Stores ``lhs + rhs`` in ``target``.
    AddI32 {
        target: Address,
        lhs: Address,
        rhs: Address,
    },
    /// Stores ``-value`` in ``target``.
    Neg { target: Address, value: Address },
    /// Copies ``value`` into ``target``.
    Copy {
        target: Address,
        value: Address,
        size: Size,
    },
    /// Reads gloabl address of ``target`` into ``dst``.
    AddressOff { dst: Address, target: Address },
    /// Reads ``*ptr`` into ``target``.
    Read {
        target: Address,
        ptr: Address,
        size: Size,
    },
    /// Writes ``value`` into ``*target``.
    Write {
        target: Address,
        value: Address,
        size: Size,
    },
    /// Stores ``lhs == rhs`` in ``target``.
    Eq {
        target: Address,
        lhs: Address,
        rhs: Address,
        size: Size,
    },
    /// Stores ``!tgt`` in ``dst``.
    Not { dst: Address, tgt: Address },
    /// Jumps to ``entry_point`` if ``value == 0``.
    ConditionalJump {
        entry_point: EntryPoint,
        value: Address,
    },
    /// Jumps to ``entry_point``.
    Jump { entry_point: EntryPoint },
    /// Calls ``function`` with ``parameters`` and stores result in ``target``.
    Call {
        target: Address,
        function: EntryPoint,
        parameters: Vec<Address>,
    },
    /// Returns from function with ``target``.
    Return { target: Address },
}

impl Statement {
    pub fn dump(&self) -> String {
        match self {
            Self::Nop => format!("nop"),
            Self::Init { target, value } => format!("init {} {:?}", target, value),
            Self::AddI32 { target, lhs, rhs } => format!("add {} {} {}", target, lhs, rhs),
            Self::Neg { target, value } => format!("neg {} {}", target, value),
            Self::Copy {
                target,
                value,
                size,
            } => format!("copy {} {} {}", target, value, size),
            Self::AddressOff { dst, target } => format!("ptr {} {}", dst, target),
            Self::Read { target, ptr, size } => format!("read {} {} {}", target, ptr, size),
            Self::Write {
                target,
                value,
                size,
            } => format!("write {} {} {}", target, value, size),
            Self::Eq {
                target,
                lhs,
                rhs,
                size,
            } => format!("eq {} {} {} {}", target, lhs, rhs, size),
            Self::Not { dst, tgt } => format!("not {} {}", dst, tgt),
            Self::ConditionalJump { entry_point, value } => {
                format!("cjmp {} {}", entry_point, value)
            }
            Self::Jump { entry_point } => format!("jmp {}", entry_point),
            Self::Call {
                target,
                function,
                parameters,
            } => format!("call {} {} {:?}", target, function, parameters),
            Self::Return { target } => format!("return {}", target),
        }
    }
}
