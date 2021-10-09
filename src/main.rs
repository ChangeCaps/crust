use std::fs::read_to_string;

use crust::{
    compiler::{targets::RuntimeCompiler, Address, Compilation, Module},
    parser,
    runtime::Runtime,
    token_stream::TokenStream,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = read_to_string("test.cr")?;

    let mut tokens = TokenStream::new(&source)?;
    let block = parser::parse_program(&mut tokens)?;

    let mut module = Module::default();

    for decl in &block.decls {
        module.add_decl(decl);
    }

    let (compilation, program) =
        Compilation::compile_program(&mut RuntimeCompiler::default(), &block, &module)?;

    compilation.dump();

    let mut runtime = Runtime::new(256);

    runtime.run_program(&program)?;

    println!("\nEAX:");
    println!("{}", runtime.stack.display_range(Address(0)..Address(4)));

    println!("EBX:");
    println!("{}", runtime.stack.display_range(Address(4)..Address(8)));

    println!("RET:");
    println!("{}", runtime.stack.display_range(Address(8)..Address(12)));

    println!("Generic registers:");
    println!(
        "{}",
        runtime
            .stack
            .display_range(Address(12)..program.data_address)
    );

    println!("Constant data:");
    println!(
        "{}",
        runtime
            .stack
            .display_range(program.data_address..program.stack_offset)
    );

    println!("Stack:");
    println!(
        "{}",
        runtime
            .stack
            .display_range(program.stack_offset..runtime.stack.end())
    );

    Ok(())
}
