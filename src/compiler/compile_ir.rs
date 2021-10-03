use std::{
    any::Any,
    collections::{BTreeMap, HashMap},
    ops::Bound,
};

use crate::{
    expr::{BinOp, Block, Expr, Literal, UnaryOp},
    path::Path,
    value::{Type, Value},
    Address, Align, Size,
};

use super::{Compiler, Statement, TypeRegistry};

#[derive(thiserror::Error, Debug)]
pub enum CompilerError {
    #[error("'{0}' not defined")]
    Undefined(Path),

    #[error("'{0:?}' not defined")]
    TypeUndefined(Type),

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

pub type CompilerResult<T> = Result<T, CompilerError>;
pub type StatementId = usize;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EntryPoint {
    Main,
    Function(Path),
    Internal(Box<EntryPoint>, usize),
}

impl std::fmt::Display for EntryPoint {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Main => write!(f, "{{main}}"),
            Self::Function(path) => path.fmt(f),
            Self::Internal(entry_point, index) => write!(f, "{}->L{}", entry_point, index),
        }
    }
}

pub struct Compilation {
    pub statements: BTreeMap<StatementId, Statement>,
    pub next_id: StatementId,
    pub entry_points: HashMap<EntryPoint, StatementId>,
    pub type_registry: TypeRegistry,
}

impl Compilation {
    pub fn new() -> Self {
        Self {
            statements: BTreeMap::new(),
            next_id: 0,
            entry_points: HashMap::new(),
            type_registry: TypeRegistry::new(),
        }
    }

    fn sorted_entry_points(&self) -> BTreeMap<StatementId, Vec<&EntryPoint>> {
        let mut entry_points = BTreeMap::<StatementId, Vec<&EntryPoint>>::new();

        for (entry_point, i) in &self.entry_points {
            entry_points.entry(*i).or_default().push(entry_point);
        }

        entry_points
    }

    pub fn compile<C: Compiler>(&self, compiler: &mut C) -> C::Output {
        let entry_points = self.sorted_entry_points();

        let mut statements = self.statements.iter().peekable();

        loop {
            let (idx, stmt) = if let Some(v) = statements.next() {
                v
            } else {
                break;
            };

            for (_, entry_points) in entry_points.range(*idx..=*idx) {
                for entry_point in entry_points {
                    compiler.entry_point((*entry_point).clone());
                }
            }

            match *stmt {
                Statement::Nop => {}
                Statement::Init { target, ref value } => compiler.init(target, value.clone()),
                Statement::AddI32 { target, lhs, rhs } => compiler.add_i32(target, lhs, rhs),
                Statement::Neg { target, value } => compiler.neg_i32(target, value),
                Statement::Copy {
                    target,
                    value,
                    size,
                } => compiler.copy(target, value, size),
                Statement::AddressOff { dst, target } => compiler.address_of(dst, target),
                Statement::Read { target, ptr, size } => compiler.read(target, ptr, size),
                Statement::Write {
                    target,
                    value,
                    size,
                } => compiler.write(target, value, size),
                Statement::Eq {
                    target,
                    lhs,
                    rhs,
                    size,
                } => compiler.eq(target, lhs, rhs, size),
                Statement::Not { dst, tgt } => compiler.not(dst, tgt),
                Statement::ConditionalJump {
                    ref entry_point,
                    value,
                } => compiler.conditional_jump(entry_point.clone(), value),
                Statement::Jump { ref entry_point } => compiler.jump(entry_point.clone()),
                Statement::Call {
                    target,
                    ref function,
                    ref parameters,
                } => compiler.call(target, function.clone(), parameters),
                _ => unimplemented!(),
            }

            if let Some((next, _)) = statements.peek() {
                if idx != *next {
                    for (_, entry_points) in
                        entry_points.range((Bound::Excluded(*idx), Bound::Excluded(**next)))
                    {
                        for entry_point in entry_points {
                            compiler.entry_point((*entry_point).clone());
                        }
                    }
                }
            } else {
                for (_, entry_points) in
                    entry_points.range((Bound::Excluded(*idx), Bound::Unbounded))
                {
                    for entry_point in entry_points {
                        compiler.entry_point((*entry_point).clone());
                    }
                }
            }
        }

        compiler.finish()
    }

    pub fn dump(&self) {
        let entry_points_sorted = self.sorted_entry_points();

        let mut entry_points = entry_points_sorted.into_iter().peekable();

        loop {
            if let Some((id, names)) = entry_points.next() {
                for name in names {
                    println!("{}:", name);
                }

                if let Some((next, _)) = entry_points.peek() {
                    for (_, statement) in self.statements.range(id..*next) {
                        println!("    {}", statement.dump());
                    }
                } else {
                    for (_, statement) in self.statements.range(id..) {
                        println!("    {}", statement.dump());
                    }
                }

                println!();
            } else {
                break;
            }
        }
    }

    pub fn next_id(&self) -> StatementId {
        self.next_id
    }

    pub fn push(&mut self, statement: Statement) -> StatementId {
        let id = self.next_id();

        self.statements.insert(id, statement);
        self.next_id += 1;

        id
    }

    pub fn compile_read(
        &mut self,
        ctx: &mut CompilationContext,
        expr: &Expr,
    ) -> CompilerResult<Value> {
        let register = self.compile_expr(ctx, expr)?;

        if register.ptr {
            let (size, align) = self.type_registry.get_size_align(&register.ty)?;

            let target = ctx.make(size, align);

            self.push(Statement::Read {
                target,
                ptr: register.address,
                size,
            });

            Ok(Value::temp(register.ty, target))
        } else {
            Ok(register)
        }
    }

    pub fn compile_expr(
        &mut self,
        ctx: &mut CompilationContext,
        expr: &Expr,
    ) -> CompilerResult<Value> {
        match expr {
            Expr::Nop => Ok(ctx.make_unit()),
            Expr::Literal(lit) => match lit {
                Literal::Integer(int, _) => {
                    let (size, align) = self.type_registry.get_size_align(&Type::I32)?;

                    let target = ctx.make(size, align);

                    self.push(Statement::Init {
                        target,
                        value: (*int as i32).to_be_bytes().to_vec(),
                    });

                    Ok(Value::temp(Type::I32, target))
                }
            },
            Expr::Paren(expr) => self.compile_expr(ctx, expr),
            Expr::Path(path) => match ctx.get(path) {
                Some(register) => Ok(register.clone()),
                None => Err(CompilerError::Undefined(path.clone())),
            },
            Expr::Let(path, value) => {
                let value = self.compile_read(ctx, value)?;

                ctx.insert(path.clone(), Value::stored(value.ty, value.address));

                Ok(ctx.make_unit())
            }
            Expr::Assign(lhs, rhs) => {
                let lhs = self.compile_expr(ctx, lhs)?;
                let rhs = self.compile_read(ctx, rhs)?;

                if lhs.ty != rhs.ty {
                    return Err(CompilerError::ExpectedType {
                        expected: lhs.ty.clone(),
                        found: rhs.ty.clone(),
                    });
                }

                let size = self.type_registry.get_size(&lhs.ty)?;

                if lhs.ptr {
                    self.push(Statement::Write {
                        target: lhs.address,
                        value: rhs.address,
                        size,
                    });
                } else {
                    self.push(Statement::Copy {
                        target: lhs.address,
                        value: rhs.address,
                        size,
                    });
                }

                Ok(ctx.make_unit())
            }
            Expr::Unary(op, expr) => {
                let expr = self.compile_read(ctx, expr)?;

                match op {
                    UnaryOp::Ref => {
                        let ty = Type::Ref(Box::new(expr.ty));

                        let (size, align) = self.type_registry.get_size_align(&ty)?;

                        let target = ctx.make(size, align);

                        self.push(Statement::AddressOff {
                            dst: target,
                            target: expr.address,
                        });

                        Ok(Value::temp(ty, target))
                    }
                    UnaryOp::Deref => {
                        if let Type::Ref(ty) = expr.ty {
                            Ok(Value::new(*ty, expr.address, false, true))
                        } else {
                            Err(CompilerError::InvalidDeref)
                        }
                    }
                    UnaryOp::Neg => {
                        if let Type::I32 = expr.ty {
                            let dst = ctx.make(4, 4);

                            self.push(Statement::Neg {
                                target: dst,
                                value: expr.address,
                            });

                            Ok(Value::temp(expr.ty, dst))
                        } else {
                            Err(CompilerError::ExpectedType {
                                expected: Type::I32,
                                found: expr.ty,
                            })
                        }
                    }
                    UnaryOp::Not => {
                        if let Type::Bool = expr.ty {
                            let dst = ctx.make(4, 4);

                            self.push(Statement::Not {
                                dst,
                                tgt: expr.address,
                            });

                            Ok(Value::temp(expr.ty, dst))
                        } else {
                            Err(CompilerError::ExpectedType {
                                expected: Type::Bool,
                                found: expr.ty,
                            })
                        }
                    }
                }
            }
            Expr::Binary(lhs, op, rhs) => {
                match op {
                    BinOp::AddEq | BinOp::SubEq | BinOp::MulEq | BinOp::DivEq => {
                        let lhs = self.compile_expr(ctx, lhs)?;
                        let rhs = self.compile_read(ctx, rhs)?;

                        if lhs.ty != rhs.ty {
                            return Err(CompilerError::ExpectedType {
                                expected: lhs.ty.clone(),
                                found: rhs.ty.clone(),
                            });
                        }

                        let (size, align) = self.type_registry.get_size_align(&lhs.ty)?;

                        match op {
                            BinOp::AddEq => {
                                match lhs.ty {
                                    Type::I32 => {
                                        if lhs.ptr {
                                            let target = ctx.make(size, align);

                                            self.push(Statement::Read {
                                                target,
                                                ptr: lhs.address,
                                                size,
                                            });

                                            self.push(Statement::AddI32 {
                                                target,
                                                lhs: target,
                                                rhs: rhs.address,
                                            });

                                            self.push(Statement::Write {
                                                target: lhs.address,
                                                value: target,
                                                size,
                                            });
                                        } else {
                                            self.push(Statement::AddI32 {
                                                target: lhs.address,
                                                lhs: lhs.address,
                                                rhs: rhs.address,
                                            });
                                        }
                                    }
                                    _ => {
                                        return Err(CompilerError::BinOpNotSupported {
                                            op: *op,
                                            ty: lhs.ty, 
                                        })
                                    }
                                }
                            }
                            _ => unreachable!(),
                        }

                        return Ok(ctx.make_unit());
                    }
                    _ => {}
                }

                let lhs = self.compile_read(ctx, lhs)?;
                let rhs = self.compile_read(ctx, rhs)?;

                if lhs.ty != rhs.ty {
                    return Err(CompilerError::ExpectedType {
                        expected: lhs.ty.clone(),
                        found: rhs.ty.clone(),
                    });
                }

                match op {
                    BinOp::Add => {
                        let (size, align) = self.type_registry.get_size_align(&lhs.ty)?;

                        let target = ctx.make(size, align);

                        match lhs.ty {
                            Type::I32 => {
                                self.push(Statement::AddI32 {
                                    target,
                                    lhs: lhs.address,
                                    rhs: rhs.address,
                                });
                            }
                            _ => {
                                return Err(CompilerError::BinOpNotSupported {
                                    op: *op,
                                    ty: lhs.ty,
                                })
                            }
                        }

                        Ok(Value::temp(lhs.ty, target))
                    }
                    BinOp::Sub => {
                        let (size, align) = self.type_registry.get_size_align(&lhs.ty)?;

                        let target = ctx.make(size, align);

                        match lhs.ty {
                            Type::I32 => {
                                self.push(Statement::AddI32 {
                                    target,
                                    lhs: lhs.address,
                                    rhs: rhs.address,
                                });
                            }
                            _ => {
                                return Err(CompilerError::BinOpNotSupported {
                                    op: *op,
                                    ty: lhs.ty,
                                })
                            }
                        }

                        self.push(Statement::Neg {
                            target,
                            value: target,
                        });

                        Ok(Value::temp(lhs.ty, target))
                    }
                    BinOp::Eq => {
                        let (size, align) = self.type_registry.get_size_align(&Type::Bool)?;

                        let target = ctx.make(size, align);

                        self.push(Statement::Eq {
                            target,
                            lhs: lhs.address,
                            rhs: rhs.address,
                            size,
                        });

                        Ok(Value::temp(Type::Bool, target))
                    }
                    _ => todo!(),
                }
            }
            Expr::Block(block) => {
                self.compile_block(ctx, block)?;

                Ok(ctx.make_unit())
            }
            Expr::If(condition, if_true, if_false) => {
                let condition = self.compile_read(ctx, condition)?;

                let end = ctx.entry_point();
                self.push(Statement::ConditionalJump {
                    entry_point: end.clone(),
                    value: condition.address,
                });

                if let Some(if_false) = if_false {
                    self.compile_expr(ctx, if_true)?;

                    let false_point = ctx.entry_point();
                    self.push(Statement::Jump {
                        entry_point: false_point.clone(),
                    });

                    self.entry_points.insert(end, self.next_id());

                    self.compile_expr(ctx, if_false)?;

                    self.entry_points.insert(false_point, self.next_id());
                } else {
                    self.compile_expr(ctx, if_true)?;

                    self.entry_points.insert(end, self.next_id());
                }

                Ok(ctx.make_unit())
            }
            Expr::While(condition, block) => {
                let loop_point = ctx.entry_point();
                let end_point = ctx.entry_point();

                self.entry_points.insert(loop_point.clone(), self.next_id());

                let condition = self.compile_read(ctx, condition)?;

                if let Type::Bool = condition.ty {
                    self.push(Statement::ConditionalJump {
                        entry_point: end_point.clone(),
                        value: condition.address,
                    });
                } else {
                    return Err(CompilerError::ExpectedType {
                        expected: Type::Bool,
                        found: condition.ty,
                    });
                }

                self.compile_expr(ctx, block)?;

                self.push(Statement::Jump {
                    entry_point: loop_point,
                });

                self.entry_points.insert(end_point, self.next_id());

                Ok(ctx.make_unit())
            }
        }
    }

    pub fn compile_block(
        &mut self,
        ctx: &mut CompilationContext,
        block: &Block,
    ) -> CompilerResult<()> {
        for expr in &block.exprs {
            self.compile_expr(ctx, expr)?;
        }

        Ok(())
    }
}

pub struct CompilationContext {
    pub entry_point: EntryPoint,
    pub path_map: HashMap<Path, Value>,
    pub next_address: Address,
    pub next_entry_point: usize,
}

impl CompilationContext {
    pub fn new(entry_point: EntryPoint) -> Self {
        Self {
            entry_point,
            path_map: HashMap::new(),
            next_address: 0,
            next_entry_point: 0,
        }
    }

    pub fn or_unit(&self, register: Option<Value>) -> Value {
        if let Some(register) = register {
            register
        } else {
            Value::temp(Type::Unit, self.next_address)
        }
    }

    pub fn entry_point(&mut self) -> EntryPoint {
        let x = self.next_entry_point;
        self.next_entry_point += 1;
        EntryPoint::Internal(Box::new(self.entry_point.clone()), x)
    }

    pub fn make(&mut self, size: Size, align: Align) -> Address {
        let address = self::align(self.next_address, align);
        self.next_address = address + size;
        address
    }

    pub fn make_unit(&self) -> Value {
        Value::temp(Type::Unit, self.next_address)
    }

    pub fn push(
        &mut self,
        path: Path,
        ty: Type,
        size: Size,
        align: Align,
    ) -> CompilerResult<Address> {
        let address = self.make(size, align);
        self.path_map.insert(path, Value::stored(ty, address));
        Ok(address)
    }

    pub fn insert(&mut self, path: Path, register: Value) {
        self.path_map.insert(path, register);
    }

    pub fn get(&mut self, path: &Path) -> Option<&Value> {
        self.path_map.get(path)
    }
}

fn align(address: Address, align: Align) -> Address {
    (address + 1) / align * align
}
