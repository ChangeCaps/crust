use std::collections::HashMap;

use crate::{
    decl::{Decl, FunctionDecl},
    path::{Path, PathSegment},
};

#[derive(Clone, Default, Debug)]
pub struct Module {
    pub modules: HashMap<String, Module>,
    pub uses: HashMap<String, Path>,
    pub functions: HashMap<String, FunctionDecl>,
}

impl Module {
    #[inline]
    pub fn add_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Function(function_decl) => {
                self.functions
                    .insert(function_decl.ident.clone(), function_decl.clone());
            }
            Decl::Module(module_decl) => {
                let mut module = Module::default();

                for decl in &module_decl.decls {
                    module.add_decl(decl);
                }

                self.modules.insert(module_decl.name.clone(), module);
            }
            Decl::Use(path) => {
                if let PathSegment::Ident(ident) = path.segments.last().unwrap() {
                    self.uses.insert(ident.clone(), path.clone());
                }
            }
        }
    }

    #[inline]
    pub fn canonicalize(&self, path: &Path) -> Option<Path> {
        let mut canonicalized = Path::default();
        let mut module = self;

        for segment in &path.segments[..path.segments.len() - 1] {
            match segment {
                PathSegment::Ident(ident) => {
                    if let Some(use_path) = module.uses.get(ident) {
                        module = self.get_module(use_path)?;

                        canonicalized = self.canonicalize(use_path)?;
                    } else {
                        if let Some(m) = self.modules.get(ident) {
                            module = m;
                        }

                        canonicalized.segments.push(segment.clone())
                    }
                }
                PathSegment::Super => {
                    canonicalized.segments.pop();
                }
                _ => {}
            }
        }

        match path.segments.last().unwrap() {
            PathSegment::Ident(ident) => {
                if let Some(path) = module.uses.get(ident) {
                    self.canonicalize(path)
                } else {
                    canonicalized
                        .segments
                        .push(PathSegment::Ident(ident.clone()));

                    Some(canonicalized)
                }
            }
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn get_module(&self, path: &Path) -> Option<&Module> {
        let path = self.canonicalize(path)?;
        let mut module = self;

        for segment in &path.segments {
            match segment {
                PathSegment::Ident(ident) => {
                    module = self.modules.get(ident)?;
                }
                _ => unreachable!(),
            }
        }

        Some(module)
    }

    #[inline]
    pub fn get_function(&self, path: &Path) -> Option<&FunctionDecl> {
        let path = self.canonicalize(path)?;
        let mut module = self;

        for segment in &path.segments[..path.segments.len() - 1] {
            match segment {
                PathSegment::Ident(ident) => module = module.modules.get(ident)?,
                _ => unreachable!(),
            }
        }

        if let PathSegment::Ident(ident) = path.segments.last()? {
            module.functions.get(ident)
        } else {
            unreachable!()
        }
    }
}
