use std::{
    any::Any,
    collections::{BTreeMap, BTreeSet, HashMap},
    ops::{Bound, RangeBounds},
};

use crate::{
    expr::{BinOp, Block, Expr, Literal, UnaryOp},
    path::Path,
};

use super::{Compiler, IRStatement, Module, Register, StackSlot, Type, TypeRegistry, Value};

#[derive(thiserror::Error, Debug)]
pub enum CompilerError {
    #[error("'{0}' not defined")]
    Undefined(Path),

    #[error("'{0}' not defined")]
    TypeUndefined(Type),

    #[error("invalid return type expected '{expected}' but found '{found}'")]
    InvalidReturnType { expected: Type, found: Type },

    #[error("invalid deref target")]
    InvalidDeref,

    #[error("all objects on the stack must have a known size")]
    UnsizedStackPush,

    #[error("cannot assign to temporary values")]
    AssignToTemp,

    #[error("expected '{expected:?}' found '{found:?}'")]
    ExpectedType { expected: Type, found: Type },

    #[error("binary operation '{op}' not supported for '{ty:?}'")]
    BinOpNotSupported { op: BinOp, ty: Type },
}

pub enum Mut<'a, T> {
    Mut(&'a mut T),
    Owned(T),
}

impl<'a, T> std::ops::Deref for Mut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Mut(t) => &**t,
            Self::Owned(t) => t,
        }
    }
}

impl<'a, T> std::ops::DerefMut for Mut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Mut(t) => *t,
            Self::Owned(t) => t,
        }
    }
}

pub type CompilerResult<T> = Result<T, CompilerError>;
pub type StatementId = usize;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EntryPoint {
    Main,
    Function(Path, u64),
    Internal(Box<EntryPoint>, usize),
}

impl EntryPoint {
    #[inline]
    pub fn is_internal(&self) -> bool {
        match self {
            Self::Internal(_, _) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn arg_count(&self) -> u64 {
        match self {
            Self::Function(_, arg_count) => *arg_count,
            _ => 0,
        }
    }
}

impl std::fmt::Display for EntryPoint {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Main => write!(f, "{{main}}"),
            Self::Function(path, slots) => {
                write!(f, "{}(", path)?;

                for i in 0..*slots {
                    if i != *slots - 1 {
                        write!(f, "arg{}, ", i)?;
                    } else {
                        write!(f, "arg{}", i)?;
                    }
                }

                write!(f, ")")
            }
            Self::Internal(entry_point, index) => write!(f, "{}->L{}", entry_point, index),
        }
    }
}

#[derive(Default)]
pub struct Compilation {
    pub statements: BTreeMap<StatementId, IRStatement>,
    pub next_id: StatementId,
    pub entry_point_map: BTreeMap<StatementId, Vec<EntryPoint>>,
    pub type_registry: TypeRegistry,
}

impl Compilation {
    #[inline]
    fn entry_points_in(
        &self,
        range: impl RangeBounds<StatementId>,
    ) -> impl Iterator<Item = &EntryPoint> {
        self.entry_point_map
            .range(range)
            .flat_map(|(_, entry_points)| entry_points)
    }

    #[inline]
    pub fn statements(&self) -> impl Iterator<Item = (Vec<&EntryPoint>, &IRStatement)> {
        let mut prev = Bound::Unbounded;

        self.statements.iter().map(move |(id, stmt)| {
            let entry_points = self.entry_points_in((prev, Bound::Included(*id))).collect();

            prev = Bound::Excluded(*id);

            (entry_points, stmt)
        })
    }

    #[inline]
    pub fn dump(&self) {
        for (entry_points, stmt) in self.statements() {
            if entry_points.len() > 0 {
                println!();
            }

            for entry_point in entry_points {
                println!("{}:", entry_point);
            }

            println!("    {}", stmt);
        }
    }

    #[inline]
    pub fn compile_program<C: Compiler>(
        compiler: &mut C,
        program: &Block,
        module: &Module,
    ) -> CompilerResult<(Self, C::Output)> {
        let mut compilation = Self::default();

        compilation.type_registry.init_primitives();

        let mut main_scope = compilation.scope(module, EntryPoint::Main);

        for expr in &program.exprs {
            main_scope.compile_expr(expr)?;
        }

        main_scope.end()?;

        main_scope.push(IRStatement::Exit { src: Register(0) });

        main_scope.compile_module(&Path::default(), module)?;

        let output = compilation.compile(compiler);

        Ok((compilation, output))
    }

    #[inline]
    pub fn compile<C: Compiler>(&mut self, compiler: &mut C) -> C::Output {
        for (entry_points, stmt) in self.statements() {
            for entry_point in entry_points {
                compiler.entry_point(entry_point.clone());
            }

            stmt.compile(compiler);
        }

        compiler.finish()
    }

    #[inline]
    fn insert_entry_point(&mut self, entry_point: EntryPoint) {
        let idx = self.statements.len();

        self.entry_point_map
            .entry(idx)
            .or_default()
            .push(entry_point.clone());
    }

    #[inline]
    pub fn scope<'a>(&'a mut self, module: &'a Module, entry_point: EntryPoint) -> Scope<'a> {
        self.insert_entry_point(entry_point.clone());

        Scope {
            compilation: self,
            module,
            base_path: Path::default(),
            return_type: Type::Void,
            returned: false,
            entry_point: entry_point.clone(),
            next_internal: Mut::Owned(0),
            variables: HashMap::new(),
            next_stack_slot: StackSlot(0),
            stack_size: 0,
            available_registers: BTreeSet::default(),
            next_register: Register(0),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub ty: Type,
    pub slot: StackSlot,
}

pub struct Scope<'a> {
    pub compilation: &'a mut Compilation,
    pub module: &'a Module,
    pub base_path: Path,
    pub return_type: Type,
    pub returned: bool,
    pub entry_point: EntryPoint,
    pub next_internal: Mut<'a, usize>,
    pub variables: HashMap<Path, Variable>,
    pub next_stack_slot: StackSlot,
    pub stack_size: u64,
    pub available_registers: BTreeSet<Register>,
    pub next_register: Register,
}

impl<'a> Scope<'a> {
    #[inline]
    pub fn sub_scope<'b>(&'b mut self) -> Scope<'b>
    where
        'a: 'b,
    {
        Scope {
            compilation: &mut *self.compilation,
            module: &*self.module,
            base_path: self.base_path.clone(),
            return_type: self.return_type.clone(),
            returned: false,
            entry_point: self.entry_point.clone(),
            next_internal: Mut::Mut(&mut self.next_internal),
            variables: self.variables.clone(),
            next_stack_slot: self.next_stack_slot,
            stack_size: 0,
            available_registers: self.available_registers.clone(),
            next_register: self.next_register,
        }
    }

    #[inline]
    fn push(&mut self, statement: IRStatement) -> StatementId {
        let id = self.compilation.next_id;
        self.compilation.statements.insert(id, statement);
        self.compilation.next_id += 1;
        id
    }

    #[inline]
    fn push_stack(&mut self) -> StackSlot {
        let slot = self.next_stack_slot.clone();
        self.next_stack_slot.0 += 1;
        self.stack_size += 1;
        slot
    }

    #[inline]
    fn push_variable(&mut self, path: Path, ty: Type) -> StackSlot {
        let slot = self.push_stack();
        self.variables.insert(
            path,
            Variable {
                ty,
                slot: slot.clone(),
            },
        );
        slot
    }

    #[inline]
    fn next_entry_point(&mut self) -> EntryPoint {
        let entry_point =
            EntryPoint::Internal(Box::new(self.entry_point.clone()), *self.next_internal);
        *self.next_internal += 1;
        entry_point
    }

    #[inline]
    fn insert_entry_point(&mut self, entry_point: EntryPoint) {
        self.compilation.insert_entry_point(entry_point);
    }

    #[inline]
    fn alloc_register(&mut self) -> Register {
        match self.available_registers.iter().next() {
            Some(register) => {
                let register = *register;
                self.available_registers.remove(&register);
                register
            }
            None => {
                let register = self.next_register;
                self.next_register.0 += 1;
                register
            }
        }
    }

    #[inline]
    fn free_register(&mut self, register: Register) {
        self.available_registers.insert(register);
    }

    #[inline]
    fn allocated_registers(&self) -> impl Iterator<Item = Register> + '_ {
        (0..self.next_register.0).into_iter().filter_map(move |r| {
            if !self.available_registers.contains(&Register(r)) {
                Some(Register(r))
            } else {
                None
            }
        })
    }

    #[inline]
    fn compile_read(&mut self, expr: &Expr) -> CompilerResult<Value> {
        let val = self.compile_expr(expr)?;

        if let Type::Ref(tgt_type) = val.ty.clone() {
            if val.implicit_dereference {
                self.push(IRStatement::Read {
                    dst: val.register,
                    src: val.register,
                });

                Ok(Value {
                    ty: *tgt_type,
                    register: val.register,
                    implicit_dereference: false,
                })
            } else {
                Ok(val)
            }
        } else {
            Ok(val)
        }
    }

    #[inline]
    pub fn compile_expr(&mut self, expr: &Expr) -> CompilerResult<Value> {
        match expr {
            Expr::Nop => Ok(Value::unused()),
            Expr::Comment(_) => Ok(Value::unused()),
            Expr::Paren(expr) => self.compile_expr(expr),
            Expr::Literal(lit) => match lit {
                Literal::Integer(int, _format) => {
                    let register = self.alloc_register();

                    self.push(IRStatement::ConstI32 {
                        dst: register,
                        src: *int as i32,
                    });

                    Ok(Value {
                        ty: Type::I32,
                        register,
                        implicit_dereference: false,
                    })
                }
                Literal::Bool(src) => {
                    let register = self.alloc_register();

                    self.push(IRStatement::ConstBool {
                        dst: register,
                        src: *src,
                    });

                    Ok(Value {
                        ty: Type::Bool,
                        register,
                        implicit_dereference: false,
                    })
                }
                Literal::Void => {
                    let register = self.alloc_register();

                    Ok(Value {
                        ty: Type::Void,
                        register,
                        implicit_dereference: false,
                    })
                }
            },
            Expr::Path(path) => {
                let func_path = self.base_path.combine(path);

                if let Some(val) = self.variables.get(path).cloned() {
                    let register = self.alloc_register();

                    self.push(IRStatement::StackPtr {
                        dst: register,
                        src: val.slot,
                    });

                    Ok(Value {
                        ty: Type::Ref(Box::new(val.ty)),
                        register,
                        implicit_dereference: true,
                    })
                } else if let Some(function) = self.module.get_function(&func_path) {
                    let register = self.alloc_register();

                    self.push(IRStatement::FuncPtr {
                        dst: register,
                        src: EntryPoint::Function(
                            self.module.canonicalize(&func_path).unwrap(),
                            function.args.len() as u64,
                        ),
                    });

                    let args = function.args.iter().map(|arg| arg.ty.clone()).collect();

                    Ok(Value {
                        ty: Type::Func(args, Box::new(function.return_type.clone())),
                        register,
                        implicit_dereference: false,
                    })
                } else {
                    Err(CompilerError::Undefined(path.clone()))
                }
            }
            Expr::Assign(tgt, src) => {
                let tgt = self.compile_expr(tgt)?;
                let src = self.compile_read(src)?;

                if let Type::Ref(tgt_type) = tgt.ty.clone() {
                    if *tgt_type != src.ty {
                        return Err(CompilerError::ExpectedType {
                            expected: *tgt_type,
                            found: src.ty,
                        });
                    }

                    if tgt.implicit_dereference {
                        self.push(IRStatement::Store {
                            dst: tgt.register,
                            src: src.register,
                        });
                    } else {
                        panic!();
                    }

                    self.free_register(tgt.register);
                    self.free_register(src.register);

                    Ok(tgt)
                } else {
                    Err(CompilerError::InvalidDeref)
                }
            }
            Expr::Let(path, src) => {
                let src = self.compile_read(src)?;

                let slot = self.push_variable(path.clone(), src.ty);

                self.push(IRStatement::Push {
                    src: src.register,
                    slot: Some(slot),
                });

                self.free_register(src.register);

                Ok(Value::unused())
            }
            Expr::Unary(op, expr) => match op {
                UnaryOp::Ref => {
                    let val = self.compile_expr(expr)?;

                    // if val is an implicitly dereferenced pointer, we can just return the pointer
                    if let Type::Ref(_) = val.ty {
                        if val.implicit_dereference {
                            return Ok(Value {
                                ty: val.ty,
                                register: val.register,
                                implicit_dereference: false,
                            });
                        }
                    }

                    let (size, _align) = self.compilation.type_registry.get_size_align(&val.ty)?;

                    let register = self.alloc_register();

                    if size > 0 {
                        let slot = self.push_stack();
                        self.push(IRStatement::Push {
                            src: val.register,
                            slot: Some(slot),
                        });

                        self.free_register(val.register);
                    }

                    Ok(Value {
                        ty: Type::Ref(Box::new(val.ty)),
                        register,
                        implicit_dereference: false,
                    })
                }
                UnaryOp::Deref => {
                    let val = self.compile_read(expr)?;

                    if let Type::Ref(_) = val.ty {
                        Ok(Value {
                            ty: val.ty,
                            register: val.register,
                            implicit_dereference: true,
                        })
                    } else {
                        Err(CompilerError::InvalidDeref)
                    }
                }
                UnaryOp::Neg => {
                    let val = self.compile_read(expr)?;

                    match val.ty {
                        Type::I32 => {
                            self.push(IRStatement::NegI32 {
                                dst: val.register,
                                src: val.register,
                            });

                            Ok(val)
                        }
                        _ => Err(CompilerError::ExpectedType {
                            expected: Type::I32,
                            found: val.ty,
                        }),
                    }
                }
                UnaryOp::Not => {
                    let val = self.compile_read(expr)?;

                    match val.ty {
                        Type::Bool => {
                            self.push(IRStatement::Not {
                                dst: val.register,
                                src: val.register,
                            });

                            Ok(val)
                        }
                        _ => Err(CompilerError::ExpectedType {
                            expected: Type::Bool,
                            found: val.ty,
                        }),
                    }
                }
            },
            Expr::Binary(lhs, op, rhs) => match op {
                op if op.is_assign() => {
                    let lhs = self.compile_expr(lhs)?;
                    let rhs = self.compile_read(rhs)?;

                    if let Type::Ref(lhs_ty) = lhs.ty {
                        match *lhs_ty {
                            Type::I32 => {
                                let register = self.alloc_register();

                                self.push(IRStatement::Read {
                                    dst: register,
                                    src: lhs.register,
                                });

                                match op {
                                    BinOp::AddEq => {
                                        self.push(IRStatement::AddI32 {
                                            dst: register,
                                            lhs: register,
                                            rhs: rhs.register,
                                        });
                                    }
                                    BinOp::SubEq => {
                                        self.push(IRStatement::SubI32 {
                                            dst: register,
                                            lhs: register,
                                            rhs: rhs.register,
                                        });
                                    }
                                    _ => unreachable!(),
                                }

                                self.push(IRStatement::Store {
                                    dst: lhs.register,
                                    src: register,
                                });

                                self.free_register(register);
                                self.free_register(lhs.register);
                                self.free_register(rhs.register);

                                Ok(Value::unused())
                            }
                            _ => unimplemented!(),
                        }
                    } else {
                        Err(CompilerError::AssignToTemp)
                    }
                }
                BinOp::Add => {
                    let lhs = self.compile_read(lhs)?;
                    let rhs = self.compile_read(rhs)?;

                    match lhs.ty {
                        Type::I32 if rhs.ty == Type::I32 => {
                            self.push(IRStatement::AddI32 {
                                dst: lhs.register,
                                lhs: lhs.register,
                                rhs: rhs.register,
                            });

                            self.free_register(rhs.register);

                            Ok(lhs)
                        }
                        _ => unimplemented!(),
                    }
                }
                BinOp::Sub => {
                    let lhs = self.compile_read(lhs)?;
                    let rhs = self.compile_read(rhs)?;

                    match lhs.ty {
                        Type::I32 if rhs.ty == Type::I32 => {
                            self.push(IRStatement::SubI32 {
                                dst: lhs.register,
                                lhs: lhs.register,
                                rhs: rhs.register,
                            });

                            self.free_register(rhs.register);

                            Ok(lhs)
                        }
                        _ => unimplemented!(),
                    }
                }
                BinOp::Eq => {
                    let lhs = self.compile_read(lhs)?;
                    let rhs = self.compile_read(rhs)?;

                    if lhs.ty != rhs.ty {
                        return Err(CompilerError::ExpectedType {
                            expected: lhs.ty,
                            found: rhs.ty,
                        });
                    }

                    match lhs.ty {
                        _ => {
                            self.push(IRStatement::Eq {
                                dst: lhs.register,
                                lhs: lhs.register,
                                rhs: rhs.register,
                            });

                            self.free_register(rhs.register);

                            Ok(Value {
                                ty: Type::Bool,
                                register: lhs.register,
                                implicit_dereference: false,
                            })
                        }
                    }
                }
                BinOp::NotEq => {
                    let lhs = self.compile_read(lhs)?;
                    let rhs = self.compile_read(rhs)?;

                    if lhs.ty != rhs.ty {
                        return Err(CompilerError::ExpectedType {
                            expected: lhs.ty,
                            found: rhs.ty,
                        });
                    }

                    match lhs.ty {
                        _ => {
                            self.push(IRStatement::NotEq {
                                dst: lhs.register,
                                lhs: lhs.register,
                                rhs: rhs.register,
                            });

                            self.free_register(rhs.register);

                            Ok(Value {
                                ty: Type::Bool,
                                register: lhs.register,
                                implicit_dereference: false,
                            })
                        }
                    }
                }
                _ => unimplemented!(),
            },
            Expr::Cast(val, ty) => {
                let val = self.compile_expr(val)?;

                if val.implicit_dereference {
                    Ok(Value {
                        ty: Type::Ref(Box::new(ty.clone())),
                        register: val.register,
                        implicit_dereference: true,
                    })
                } else {
                    Ok(Value {
                        ty: ty.clone(),
                        register: val.register,
                        implicit_dereference: false,
                    })
                }
            }
            Expr::If(condition, true_expr, false_expr) => {
                let val = self.compile_read(condition)?;

                if val.ty != Type::Bool {
                    return Err(CompilerError::ExpectedType {
                        expected: Type::Bool,
                        found: val.ty,
                    });
                }

                if let Some(false_expr) = false_expr {
                    let false_point = self.next_entry_point();

                    self.push(IRStatement::JmpNZ {
                        dst: false_point.clone(),
                        src: val.register,
                    });

                    let mut scope = self.sub_scope();
                    scope.compile_expr(true_expr)?;
                    scope.end()?;

                    let returned = scope.returned;

                    let end_point = self.next_entry_point();

                    self.push(IRStatement::Jmp {
                        dst: end_point.clone(),
                    });

                    self.insert_entry_point(false_point);

                    let mut scope = self.sub_scope();
                    scope.compile_expr(false_expr)?;
                    scope.end()?;

                    self.returned = scope.returned && returned;

                    self.insert_entry_point(end_point);

                    self.push(IRStatement::Nop {});

                    self.free_register(val.register);

                    Ok(Value::unused())
                } else {
                    let end_point = self.next_entry_point();

                    self.push(IRStatement::JmpNZ {
                        dst: end_point.clone(),
                        src: val.register,
                    });

                    let mut scope = self.sub_scope();
                    scope.compile_expr(true_expr)?;
                    scope.end()?;

                    self.insert_entry_point(end_point);

                    self.push(IRStatement::Nop {});

                    self.free_register(val.register);

                    Ok(Value::unused())
                }
            }
            Expr::Block(block) => {
                let mut module = self.module.clone();

                for decl in &block.decls {
                    module.add_decl(decl);
                }

                let mut scope = self.sub_scope();

                for expr in &block.exprs {
                    scope.compile_expr(expr)?;
                }

                self.returned |= scope.returned;

                Ok(Value::unused())
            }
            Expr::While(condition, expr) => {
                let end_point = self.next_entry_point();
                let loop_point = self.next_entry_point();

                self.insert_entry_point(loop_point.clone());

                let val = self.compile_read(condition)?;

                self.push(IRStatement::JmpNZ {
                    dst: end_point.clone(),
                    src: val.register,
                });

                let mut scope = self.sub_scope();
                scope.compile_read(expr)?;
                scope.end()?;

                self.push(IRStatement::Jmp { dst: loop_point });

                self.insert_entry_point(end_point);

                self.push(IRStatement::Nop {});

                self.free_register(val.register);

                Ok(Value::unused())
            }
            Expr::Call(func, arg_exprs) => {
                let func = self.compile_read(func)?;

                let (arg_types, return_type) = if let Type::Func(args, return_type) = func.ty {
                    (args, return_type)
                } else {
                    return Err(CompilerError::ExpectedType {
                        expected: Type::Func(Vec::new(), Box::new(Type::Void)),
                        found: func.ty,
                    });
                };

                if arg_types.len() != arg_exprs.len() {
                    todo!()
                }

                let mut args = Vec::with_capacity(arg_exprs.len());

                for (expr, ty) in arg_exprs.iter().zip(arg_types) {
                    let arg = self.compile_read(expr)?;

                    if arg.ty != ty {
                        return Err(CompilerError::ExpectedType {
                            expected: ty,
                            found: arg.ty,
                        });
                    }

                    args.push(arg.register);
                }

                for arg in &args {
                    self.free_register(*arg);
                }

                for register in self.allocated_registers().collect::<Vec<_>>() {
                    if register != func.register {
                        self.push(IRStatement::Push {
                            src: register,
                            slot: None,
                        });
                    }
                }

                self.push(IRStatement::Call {
                    dst: func.register,
                    func: func.register,
                    args,
                });

                for register in self
                    .allocated_registers()
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                {
                    if register != func.register {
                        self.push(IRStatement::Pop {
                            dst: Some(register),
                        });
                    }
                }

                Ok(Value {
                    ty: *return_type,
                    register: func.register,
                    implicit_dereference: false,
                })
            }
            Expr::Return(val) => {
                let val = self.compile_read(val)?;

                self.end()?;

                self.push(IRStatement::Ret { src: val.register });

                self.free_register(val.register);

                self.returned = true;

                Ok(Value::unused())
            }
        }
    }

    pub fn end(&mut self) -> CompilerResult<()> {
        for _ in 0..self.stack_size {
            self.push(IRStatement::Pop { dst: None });
        }

        Ok(())
    }

    pub fn compile_module(&mut self, path: &Path, module: &Module) -> CompilerResult<()> {
        for (segment, module) in &module.modules {
            self.compile_module(&path.join(segment.clone()), module)?;
        }

        for (ident, function) in &module.functions {
            let entry_point = EntryPoint::Function(path.join(ident), function.args.len() as u64);
            let mut scope = self.compilation.scope(self.module, entry_point.clone());
            scope.return_type = function.return_type.clone();
            scope.base_path = path.clone();

            let mut args = Vec::new();

            for arg in &function.args {
                let slot = scope.push_variable(Path::ident(&arg.ident), arg.ty.clone());

                args.push(slot);
            }

            scope.compile_expr(&function.expr)?;

            if !scope.returned {
                if scope.return_type != Type::Void {
                    return Err(CompilerError::InvalidReturnType {
                        expected: scope.return_type.clone(),
                        found: Type::Void,
                    });
                }

                scope.end()?;

                let register = scope.alloc_register();

                scope.push(IRStatement::Ret { src: register });

                scope.free_register(register);
            }
        }

        Ok(())
    }
}
