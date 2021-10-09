use std::{any::Any, collections::HashMap};

use super::{CompilerError, CompilerResult, Size, Type};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeInfo {
    pub size: Option<Size>,
    pub align: u64,
}

#[derive(Default)]
pub struct TypeRegistry {
    types: HashMap<Type, TypeInfo>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        let mut registry = Self::default();
        registry.init_primitives();
        registry
    }

    pub fn init_primitives(&mut self) {
        self.init(
            Type::I32,
            TypeInfo {
                size: Some(4),
                align: 4,
            },
        );
        self.init(
            Type::F32,
            TypeInfo {
                size: Some(4),
                align: 4,
            },
        );
        self.init(
            Type::Str,
            TypeInfo {
                size: None,
                align: 4,
            },
        );
        self.init(
            Type::Bool,
            TypeInfo {
                size: Some(4),
                align: 4,
            },
        );
        self.init(
            Type::Void,
            TypeInfo {
                size: Some(0),
                align: 4,
            },
        );
    }

    pub fn get_size_align(&mut self, ty: &Type) -> CompilerResult<(Size, u64)> {
        let info = self.get(ty)?;
        Ok((
            info.size.ok_or_else(|| CompilerError::UnsizedStackPush)?,
            info.align,
        ))
    }

    pub fn get_size(&mut self, ty: &Type) -> CompilerResult<Size> {
        self.get(ty)?
            .size
            .ok_or_else(|| CompilerError::UnsizedStackPush)
    }

    pub fn get(&mut self, ty: &Type) -> CompilerResult<&TypeInfo> {
        if self.types.contains_key(ty) {
            return Ok(self.types.get(ty).unwrap());
        }

        match ty {
            Type::Ref(inner) => {
                self.init(
                    inner.as_ref().clone(),
                    TypeInfo {
                        size: Some(4),
                        align: 4,
                    },
                );

                Ok(self.types.get(inner).unwrap())
            }
            Type::Func(_, _) => {
                self.init(
                    ty.clone(),
                    TypeInfo {
                        size: Some(4),
                        align: 4,
                    },
                );

                Ok(self.types.get(ty).unwrap())
            }
            _ => Err(CompilerError::TypeUndefined(ty.clone())),
        }
    }

    pub fn init_ref(&mut self, ty: Type) {
        self.types
            .entry(Type::Ref(Box::new(ty)))
            .or_insert_with(|| TypeInfo {
                size: Some(4),
                align: 4,
            });
    }

    pub fn init(&mut self, register_type: Type, info: TypeInfo) {
        self.types.insert(register_type, info);
    }
}
