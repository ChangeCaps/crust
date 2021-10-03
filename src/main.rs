use std::fs::read_to_string;

use crust::{
    compiler::{
        targets::{self, RuntimeCompiler},
        Compilation, CompilationContext, EntryPoint,
    },
    parser,
    path::Path,
    runtime::Runtime,
    token_stream::TokenStream,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = read_to_string("test.cst")?;

    let mut tokens = TokenStream::new(&source)?;
    let block = parser::parse_program(&mut tokens)?;

    let mut compilation = Compilation::new();
    let mut ctx = CompilationContext::new(EntryPoint::Main);

    compilation.compile_block(&mut ctx, &block)?;

    compilation.entry_points.insert(EntryPoint::Main, 0);

    compilation.dump();

    let program = compilation.compile(&mut RuntimeCompiler::default());

    let mut runtime = Runtime::new(64);

    runtime.run_program(&program)?;

    println!("{}", runtime.stack);

    let x_address = ctx.get(&Path::ident("x")).unwrap().address + program.data.len();
    let x = runtime.stack.read_i32(x_address)?;

    println!("x = {}", x);

    Ok(())
}
