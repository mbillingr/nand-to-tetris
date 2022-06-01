/**
Read assembly code from stdin and write binary code to stdout
**/
use nand_to_tetris::chapter06_assembler::assembler::assemble;
use std::io;
use std::io::Read;

fn main() -> Result<(), String> {
    let mut buffer = vec![];
    io::stdin()
        .read_to_end(&mut buffer)
        .map_err(|e| e.to_string())?;

    let src = String::from_utf8(buffer).map_err(|e| e.to_string())?;

    let binary = assemble(&src)?;

    for instruction in binary {
        println!("{:016b}", instruction);
    }

    Ok(())
}
