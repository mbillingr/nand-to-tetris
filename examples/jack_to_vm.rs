use nand_to_tetris::chapter08_vm::parser::Command;
/**
Read vm code from a file or directory (multiple .vm files)
and write unoptimized assembly code to stdout.
**/
use nand_to_tetris::chapter11_compiler::compiler::Compiler;
use nand_to_tetris::file_utils::file_or_files_in_dir;
use std::env;
use std::fs;
use std::io::Write;

fn main() -> Result<(), String> {
    let mut args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Jack Analyzer.\nUsage:\n    jack_xml <input-path>");
        return Ok(());
    }
    let path = args.pop().unwrap();

    let input_files = file_or_files_in_dir(&path, "jack")?;

    for input_file in input_files {
        let buffer = fs::read_to_string(&input_file).map_err(|e| e.to_string())?;

        let compiler = Compiler::compile_source(&buffer)?;

        let output_file = input_file.with_extension("vm");
        println!("writing output to {:?}", output_file);
        let mut f = fs::File::create(output_file).map_err(|e| e.to_string())?;

        for op in compiler.code() {
            match op {
                Command::Function(_, _) => writeln!(f).map_err(|e| e.to_string())?,
                _ => {}
            }
            writeln!(f, "{}", op).map_err(|e| e.to_string())?;
        }
    }

    Ok(())
}
