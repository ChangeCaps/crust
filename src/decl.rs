use crate::{compiler::Type, expr::Expr, path::Path};

#[derive(Clone, Debug)]
pub struct FunctionArg {
    pub ident: String,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub ident: String,
    pub return_type: Type,
    pub args: Vec<FunctionArg>,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ModuleDecl {
    pub name: String,
    pub decls: Vec<Decl>,
}

#[derive(Clone, Debug)]
pub enum Decl {
    Function(FunctionDecl),
    Module(ModuleDecl),
    Use(Path),
}
