use super::{Align, Compiler, EntryPoint, Register, Size, StackSlot};

macro_rules! statement {
    ( $( $stmt:ident { $( $name:ident : $ty:ty $( => $expr:expr )? , )* } $(-> $fn:ident)? , )* ) => {
        #[derive(Clone, Debug)]
        pub enum IRStatement {
            $(
                $stmt { $($name: $ty),* },
            )*
        }

        impl std::fmt::Display for IRStatement {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$stmt { $($name),* } => {
                            write!(f, "{}", stringify!($stmt).to_lowercase())?;

                            $(
                                #[allow(unused)]
                                let val = std::iter::once($name);

                                $(
                                    let val = $expr;
                                )?

                                for val in val {
                                    write!(f, " {}", val)?;
                                }
                            )*
                        },
                    )*
                }

                Ok(())
            }
        }

        impl IRStatement {
            #[inline]
            pub fn compile<C: Compiler>(&self, compiler: &mut C) {
                match self {
                    $(
                        Self::$stmt { $($name),* } => {
                            #[allow(unused)]
                            let mut f = |f: fn(&mut C, $($ty),*)| {
                                f(compiler, $($name.clone()),*);
                            };

                            $(f(C::$fn);)?
                        },
                    )*
                }
            }
        }
    };
}

statement! {
    Nop {},

    /* memory */

    Push {
        src: Register,
        slot: Option<StackSlot> => slot.iter(),
    } -> push,
    Pop {
        dst: Option<Register> => dst.iter(),
    } -> pop,
    Mov {
        dst: Register,
        src: Register,
    } -> mov,
    Copy {
        dst: Register,
        src: Register,
        size: Size,
    } -> copy,
    Read {
        dst: Register,
        src: Register,
    } -> read,
    Store {
        dst: Register,
        src: Register,
    } -> store,
    StackPtr {
        dst: Register,
        src: StackSlot,
    } -> stack_ptr,
    FuncPtr {
        dst: Register,
        src: EntryPoint,
    } -> func_ptr,
    Zero {
        dst: Register,
    } -> zero,

    /* boolean logic */

    ConstBool {
        dst: Register,
        src: bool,
    } -> const_bool,
    Not {
        dst: Register,
        src: Register,
    } -> not,
    Eq {
        dst: Register,
        lhs: Register,
        rhs: Register,
    } -> eq,
    NotEq {
        dst: Register,
        lhs: Register,
        rhs: Register,
    } -> not_eq,

    /* i32 */

    ConstI32 {
        dst: Register,
        src: i32,
    } -> const_i32,
    NegI32 {
        dst: Register,
        src: Register,
    } -> neg_i32,
    AddI32 {
        dst: Register,
        lhs: Register,
        rhs: Register,
    } -> add_i32,
    SubI32 {
        dst: Register,
        lhs: Register,
        rhs: Register,
    } -> sub_i32,

    /* control flow */

    Jmp {
        dst: EntryPoint,
    } -> jmp,
    JmpNZ {
        dst: EntryPoint,
        src: Register,
    } -> jmp_nz,
    Call {
        dst: Register,
        func: Register,
        args: Vec<Register> => args.iter(),
    } -> call,
    Ret {
        src: Register,
    } -> ret,
    Exit {
        src: Register,
    } -> exit,
}
