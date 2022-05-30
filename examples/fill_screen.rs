use nand_to_tetris::chapter01_boolean_logic::{
    Bit,
    Bit::{I, O},
};
use nand_to_tetris::chapter02_boolean_arithmetic::bus_as_number;
use nand_to_tetris::chapter05_computer_architecture::{make_screen, Computer};
use nand_to_tetris::hardware::{ClockHandler, SystemBuilder, Wire};
use nand_to_tetris::{let_wires, op};
use std::sync::Arc;

fn main() {
    let instructions = [
        // ptr = 0x4000 (scr)
        bus_as_number(&op!(@0x4000)),
        bus_as_number(&op!(D = (A))),
        bus_as_number(&op!(@0x0)),
        bus_as_number(&op!(M = (D))),
        // if kbd==0 then fillval = 0 else fillval = -1
        bus_as_number(&op!(@0x6000)),
        bus_as_number(&op!(D = (M))),
        bus_as_number(&op!(@12)),
        bus_as_number(&op!((D);JNE)),
        // (THEN=8)
        bus_as_number(&op!(@0x1)),
        bus_as_number(&op!(M = (0))),
        bus_as_number(&op!(@14)),
        bus_as_number(&op!((0);JMP)),
        // (ELSE=12)
        bus_as_number(&op!(@0x1)),
        bus_as_number(&op!(M = (-1))),
        // (ENDIF=14)
        // R[ptr] = R[fillval]
        bus_as_number(&op!(@0x1)),
        bus_as_number(&op!(D = (M))),
        bus_as_number(&op!(@0x0)),
        bus_as_number(&op!(A = (M))),
        bus_as_number(&op!(M = (D))),
        // ptr = 0x4000 + (ptr + 1) % 0x2000 = 0x4000 | (ptr+1) & 0x1FFF
        bus_as_number(&op!(D = (A + 1))),
        bus_as_number(&op!(@0x1FFF)),
        bus_as_number(&op!(D = (D & A))),
        bus_as_number(&op!(@0x4000)),
        bus_as_number(&op!(D = (A | D))),
        bus_as_number(&op!(@0x0)),
        bus_as_number(&op!(M = (D))),
        bus_as_number(&op!(@4)),
        bus_as_number(&op!((0);JMP)),
    ];

    let mut rom_data = vec![0; 32738];
    for (r, i) in rom_data.iter_mut().zip(instructions) {
        *r = i;
    }

    let mut sb = SystemBuilder::new();

    let_wires!(reset);
    let comp = Computer::new(&mut sb, "HACK", &reset, Arc::new(rom_data));

    let sys = sb.build().unwrap();
    sys.simulate();

    let window = make_screen(comp.screen.clone(), comp.keyboard.clone());

    // simulate a sufficient number of cycles
    while !window.is_finished() {
        sys.cycle();
        sys.simulate();
    }

    window.join().unwrap()
}
