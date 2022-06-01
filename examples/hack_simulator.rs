/**
Read binary ROM content (such as emitted by hasm) from stdin and simulate
running the hack hardware.
 **/
use nand_to_tetris::chapter05_computer_architecture::{make_screen, Computer};
use nand_to_tetris::hardware::{ClockHandler, SystemBuilder, Wire};
use nand_to_tetris::let_wires;
use std::io;
use std::io::Read;
use std::sync::Arc;

fn main() -> Result<(), String> {
    let mut buffer = vec![];
    io::stdin()
        .read_to_end(&mut buffer)
        .map_err(|e| e.to_string())?;

    let instructions = std::str::from_utf8(&buffer).map_err(|e| e.to_string())?;

    let mut rom_data = vec![0; 32738];
    for (i, line) in instructions.lines().enumerate() {
        rom_data[i] = u16::from_str_radix(line, 2).map_err(|e| e.to_string())?;
    }

    let mut sb = SystemBuilder::new();

    let_wires!(reset);
    let comp = Computer::new(&mut sb, "HACK", &reset, Arc::new(rom_data));

    let sys = sb.build().map_err(|e| format!("{:?}", e))?;
    sys.simulate();

    let window = make_screen(comp.screen.clone(), comp.keyboard.clone());

    let mut n_cycles: usize = 0;
    while !window.is_finished() {
        sys.cycle();
        sys.simulate();
        n_cycles += 1;
    }

    window.join().map_err(|e| format!("{:?}", e))?;

    println!("simulated {} cycles", n_cycles);
    println!("MEMORY DUMP (first few words):");
    println!("ADDR    CH  BIN               HEX     DEC");
    for i in 0..32 {
        let x = comp.ram.read().unwrap()[i];
        println!(
            "0x{:04x}  {}{}  {:016b}  {:04X}  {:5}",
            i,
            safe_char((x / 256) as u8),
            safe_char((x % 256) as u8),
            x,
            x,
            x
        );
    }

    Ok(())
}

fn safe_char(x: u8) -> char {
    let ch = x as char;
    if ch.is_control() {
        return ' ';
    }

    ch
}
