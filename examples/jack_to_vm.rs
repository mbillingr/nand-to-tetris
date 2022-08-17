/**
Read vm code from a file or directory (multiple .vm files)
and write unoptimized assembly code to stdout.
**/
use nand_to_tetris::chapter10_parser::parser;
use nand_to_tetris::chapter10_parser::parser::ClassVarDec;
use nand_to_tetris::chapter10_parser::tokenizer::JackTokenizer;
use nand_to_tetris::file_utils::file_or_files_in_dir;
use std::env;
use std::fs;
use nand_to_tetris::chapter11_compiler::compiler::Compiler;

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
        let lexer = JackTokenizer::new(&buffer);

        let parse_tree = parser::class(lexer).map_err(|e| format!("{:?}", e))?.0;

        let mut compiler = Compiler::new();
        compiler.compile_class(parse_tree)?;
        println!("{:?}", compiler.code());

        //let output_file = input_file.with_extension("xml");
    }

    Ok(())
}
