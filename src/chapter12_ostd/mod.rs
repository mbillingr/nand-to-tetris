use crate::chapter06_assembler::assembler::{assemble, SP};
use crate::chapter07_vm::parser::{Command as StackCmd, Segment};
use crate::chapter08_vm::parser::Command;
use crate::chapter08_vm::translator::CodeGenerator;
use crate::chapter11_compiler::compiler::Compiler;
use crate::computer_emulator::Computer;
use std::fs;

#[cfg(test)]
mod test_arrayy;
#[cfg(test)]
mod test_math;
#[cfg(test)]
mod test_memory;
#[cfg(test)]
mod test_string;

pub fn eval(modules: &[&str], func: &str, args: &[u16]) -> Result<u16, String> {
    let mut asm = String::new();
    let mut code_gen = CodeGenerator::new("VM");
    asm += &code_gen.gen_bootstrap();

    for module in [
        "std/Sys",
        "std/Array",
        "std/Math",
        "std/Memory",
        "std/Output",
        "std/Screen",
        "std/String",
    ]
    .iter()
    .chain(modules)
    {
        let buffer =
            fs::read_to_string(format!("content/jack/{module}.jack")).map_err(|e| e.to_string())?;
        let compiler = Compiler::compile_source(&buffer)?;

        let mod_name = module
            .rsplitn(2, '/')
            .next()
            .unwrap()
            .split('.')
            .next()
            .unwrap();
        code_gen.set_module_name(mod_name);

        asm += &code_gen.translate(compiler.code().iter().copied().map(Ok))?;
    }

    let mut init = vec![Command::Function("Main.main", 0)];

    for arg in args {
        init.push(Command::Stack(StackCmd::Push(Segment::Constant, *arg)));
    }

    init.push(Command::Call(func, args.len() as u16));
    init.push(Command::Halt);

    asm += &code_gen.translate(init.into_iter().map(Ok))?;

    let binary = assemble(&asm)?;

    let mut emu = Computer::new(binary);

    emu.run(usize::MAX);

    let sp = emu.get_ram(SP);
    Ok(emu.get_ram(sp - 1))
}
