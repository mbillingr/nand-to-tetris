use std::collections::HashMap;
use nand_to_tetris::chapter08_vm::translator::CodeGenerator;
use nand_to_tetris::chapter07_vm::parser::{Command as StackCmd, Segment};
/**
Read vm function definitions from stdin, then run Sys.init()
 **/
use std::io;
use std::io::Read;
use std::sync::Arc;
use nand_to_tetris::chapter08_vm::parser::Command;

fn main() -> Result<(), String> {
    let mut buffer = vec![];
    io::stdin()
        .read_to_end(&mut buffer)
        .map_err(|e| e.to_string())?;

    let src = String::from_utf8(buffer).map_err(|e| e.to_string())?;

    let vm_instructions = CodeGenerator::parse(&src);

    let mut functions = HashMap::new();

    let mut ops = vec![];
    let mut function_name = "";
    let mut n_locals = 0;

    for op in vm_instructions {
        match op? {
            Command::Function(name, nl) => {
                if !ops.is_empty() {
                    functions.insert(function_name, (n_locals, std::mem::replace(&mut ops, vec![])));
                }
                function_name = name;
                n_locals = nl;
            }
            op => ops.push(op),
        }
    }

    functions.insert(function_name, (n_locals, std::mem::replace(&mut ops, vec![])));

    //let iodev = Arc::new(IoDevice::new());

    let mut stack = vec![];

    let (n_locals, code) = &functions["Sys.init"];

    let mut locals = vec![0; *n_locals as usize];
    let mut code = code.as_slice();

    let mut ip = 0;
    loop {
        let op = &code[ip];
        match op {
            Command::Stack(StackCmd::Push(Segment::Constant, val)) => stack.push(*val),
            _ => todo!("{}", op),
        }
    }


    Ok(())
}
