use nand_to_tetris::computer_emulator::{Computer, IoDevice};
/**
Read binary ROM content (such as emitted by hasm) from stdin and run it in the hack emulator.
 **/
use std::io;
use std::io::Read;
use std::sync::Arc;

fn main() -> Result<(), String> {
    let mut buffer = vec![];
    io::stdin()
        .read_to_end(&mut buffer)
        .map_err(|e| e.to_string())?;

    let instructions = std::str::from_utf8(&buffer).map_err(|e| e.to_string())?;

    let mut rom_data = vec![];
    for line in instructions.lines() {
        rom_data.push(u16::from_str_radix(line, 2).map_err(|e| e.to_string())?);
    }

    let iodev = Arc::new(IoDevice::new());

    let mut emu = Computer::new(rom_data);
    emu.set_io(iodev.clone());

    emu.run(usize::MAX);

    println!("MEMORY DUMP (first few words):");
    println!("ADDR    CH  BIN               HEX     DEC");
    for i in 0..32 {
        let x = emu.get_ram(i);
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

    if let Ok(iodev) = Arc::try_unwrap(iodev) {
        iodev.join()?;
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
