use crate::chapter01_boolean_logic::Bit::{I, O};
use crate::chapter01_boolean_logic::{
    make_and, make_demux_multi, make_mux_bus, make_mux_multi, make_not, make_or, make_or_reduce,
    Bit,
};
use crate::chapter02_boolean_arithmetic::make_alu;
use crate::chapter03_memory::{
    make_counter, make_memory_device, make_primitive_ramn, make_register, make_rom_device,
    MemoryDevice,
};
use crate::hardware::{make_constant, SystemBuilder, Wire};
use std::cell::RefCell;
use std::sync::Arc;

pub struct Cpu {
    pub dst_a: Wire<Bit>,
    pub a_load: Wire<Bit>,
    pub a_src: Vec<Wire<Bit>>,
    pub a_val: Vec<Wire<Bit>>,
    pub d_load: Wire<Bit>,
    pub d_src: Vec<Wire<Bit>>,
    pub d_val: Vec<Wire<Bit>>,
    pub jump: Wire<Bit>,
}

pub fn make_cpu(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    instruction: &[Wire<Bit>],
    inval: &[Wire<Bit>],
    reset: &Wire<Bit>,
    outval: &[Wire<Bit>],
    outaddr: &[Wire<Bit>],
    write: &Wire<Bit>,
    pc: &[Wire<Bit>],
) -> Cpu {
    assert_eq!(instruction.len(), 16);
    assert_eq!(inval.len(), 16);
    assert_eq!(outval.len(), 16);
    assert_eq!(outaddr.len(), 15);
    assert_eq!(pc.len(), 15);
    let name = name.into();

    // temporary constants
    let_wires!(zero, one);
    make_constant(sb, "zero", O, &zero);
    make_constant(sb, "one", I, &one);

    let jjj = &instruction[0..3];
    let ddd = &instruction[3..6];
    let cccccc = &instruction[6..12];
    let a = &instruction[12];
    let c_instruction = &instruction[15];

    let no = &cccccc[0];
    let f = &cccccc[1];
    let ny = &cccccc[2];
    let zy = &cccccc[3];
    let nx = &cccccc[4];
    let zx = &cccccc[5];

    let dst_m = &ddd[0];
    let dst_d = &ddd[1];
    let dst_a = &ddd[2];

    let jmp_pos = &jjj[0];
    let jmp_zero = &jjj[1];
    let jmp_neg = &jjj[2];

    let_wires!(a_instruction);
    make_not(
        sb,
        format!("{}.a-instruction", name),
        c_instruction,
        &a_instruction,
    );

    // write output?
    make_and(sb, format!("{}.write", name), c_instruction, dst_m, write);

    // connect the 15 lower bits of A register output to cpu output address
    let_wires!(a_msb);
    let mut a_val = outaddr.to_vec();
    a_val.push(a_msb);

    // A register
    let_wires!(a_load);
    let_buses!(a_src[16]);
    make_or(
        sb,
        format!("{}.a-load", name),
        &a_instruction,
        dst_a,
        &a_load,
    );
    make_mux_bus(
        sb,
        format!("{}.a-sel", name),
        outval,
        instruction,
        &a_instruction,
        &a_src,
    );
    make_register(sb, format!("{}.RA", name), &a_src, &a_load, &a_val);

    // D register
    let d_src = outval;
    let_wires!(d_load);
    let_buses!(d_val[16]);
    make_and(
        sb,
        format!("{}.d-load", name),
        &c_instruction,
        dst_d,
        &d_load,
    );
    make_register(sb, format!("{}.RD", name), d_src, &d_load, &d_val);

    // select alu input
    let_buses!(a_or_m[16]);
    make_mux_bus(sb, format!("{}.a/m", name), &a_val, inval, a, &a_or_m);

    // alu
    let_wires!(zr, ng);
    make_alu(
        sb,
        format!("{}.alu", name),
        &d_val,
        &a_or_m,
        zx,
        nx,
        zy,
        ny,
        f,
        no,
        outval,
        &zr,
        &ng,
    );

    let_wires!(ps, nps);
    make_or(sb, format!("{}.valnpos?", name), &zr, &ng, &&nps);
    make_not(sb, format!("{}.valpos?", name), &nps, &ps);

    // jump logic
    let_wires!(jn, jp, jz, jump, really_jump);
    make_and(sb, format!("{}.jmp+", name), jmp_pos, &ps, &jp);
    make_and(sb, format!("{}.jmp0", name), jmp_zero, &zr, &jz);
    make_and(sb, format!("{}.jmp-", name), jmp_neg, &ng, &jn);
    make_or_reduce(sb, format!("{}.jmp", name), &[jp, jz, jn], &jump);
    make_and(
        sb,
        format!("{}.jmp?", name),
        &jump,
        c_instruction,
        &really_jump,
    );

    // program counter
    make_counter(
        sb,
        format!("{}.pc", name),
        &really_jump,
        &one,
        reset,
        outaddr,
        pc,
    );

    Cpu {
        dst_a: dst_a.clone(),
        a_load,
        a_src,
        a_val,
        d_load,
        d_src: d_src.to_vec(),
        d_val,
        jump,
    }
}

pub fn make_memory(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    load: &Wire<Bit>,
    inval: &[Wire<Bit>],
    addr: &[Wire<Bit>],
    outval: &[Wire<Bit>],
    screen: &Arc<dyn MemoryDevice>,
    keyboard: &Arc<dyn MemoryDevice>,
) -> Arc<RefCell<Vec<u16>>> {
    assert_eq!(inval.len(), outval.len());
    let name = name.into();
    let width = inval.len();

    let sel = &addr[13..15];

    let_wires!(ram_load, ram0_load, ram1_load, scr_load, kbd_load_ignore);
    make_demux_multi(
        sb,
        format!("{}.load-ram/dev", name),
        load,
        sel,
        &[
            ram0_load.clone(),
            scr_load.clone(),
            ram1_load.clone(),
            kbd_load_ignore,
        ],
    );
    make_or(
        sb,
        format!("{}.load-ram", name),
        &ram0_load,
        &ram1_load,
        &ram_load,
    );

    let_buses!(ram_out[width], scr_out[width], kbd_out[width]);

    make_mux_multi(
        sb,
        format!("{}.out-ram/dev", name),
        &[&ram_out, &scr_out, &ram_out, &kbd_out],
        sel,
        outval,
    );

    let ram = make_primitive_ramn(
        sb,
        format!("{}.ram16k", name),
        14,
        addr,
        inval,
        &ram_load,
        &ram_out,
    );

    make_memory_device(
        sb,
        format!("{}.screen", name),
        addr,
        inval,
        &scr_load,
        &scr_out,
        screen.clone(),
    );

    let_wires!(kbd_load); // always zero, because we never write to the keyboard
    make_memory_device(
        sb,
        format!("{}.keyboard", name),
        addr,
        inval,
        &kbd_load,
        &kbd_out,
        keyboard.clone(),
    );

    ram
}

pub struct Computer {
    pub ram: Arc<RefCell<Vec<u16>>>,
    pub screen: Arc<RefCell<Vec<u16>>>,
    pub keyboard: Arc<RefCell<Vec<u16>>>,
    pub cpu: Cpu,
    pub pc: Vec<Wire<Bit>>,
    pub instruction: Vec<Wire<Bit>>,
    pub memval: Vec<Wire<Bit>>,
}

impl Computer {
    pub fn new(
        sb: &mut SystemBuilder<Bit>,
        name: impl Into<String>,
        reset: &Wire<Bit>,
        rom_data: Arc<Vec<u16>>,
    ) -> Self {
        let name = name.into();

        let screen = Arc::new(RefCell::new(vec![0; 0x2000]));
        let keyboard = Arc::new(RefCell::new(vec![65; 1]));

        let_wires!(write_m);
        let_buses!(out_m[16], in_m[16], addr_m[15]);
        let_buses!(instruction[16], pc[15]);

        //write_m.watch(true);

        let scr: Arc<dyn MemoryDevice> = screen.clone();
        let kbd: Arc<dyn MemoryDevice> = keyboard.clone();
        let ram = make_memory(
            sb,
            format!("{}.memory", name),
            &write_m,
            &out_m,
            &addr_m,
            &in_m,
            &scr,
            &kbd,
        );

        let cpu = make_cpu(
            sb,
            format!("{}.cpu", name),
            &instruction,
            &in_m,
            reset,
            &out_m,
            &addr_m,
            &write_m,
            &pc,
        );

        make_rom_device(sb, format!("{}.rom", name), &pc, &instruction, rom_data);

        Computer {
            ram,
            screen,
            keyboard,
            cpu,
            pc,
            instruction,
            memval: in_m,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chapter02_boolean_arithmetic::bus_as_number;
    use crate::hardware::{BusApi, ClockHandler};
    use std::cell::RefCell;
    use Bit::{I, O};

    macro_rules! op {
        // A-instruction
        (@$value:expr) => {{
            assert!($value >= 0);
            assert!($value <= 32767);
            let mut n: u16 = $value;
            let mut v = Vec::with_capacity(16);
            for _ in 0..15 {
                if n % 2 == 1 {
                    v.push(Bit::I);
                } else {
                    v.push(Bit::O);
                }
                n /= 2;
            }
            v.push(O);
            v
        }};

        // C-instruction (full)
        ($dest:tt = $comp:tt ; $jump:tt) => {{
            let mut op = Vec::with_capacity(16);
            op!(@jump, op, $jump);
            op!(@dest, op, $dest);
            op!(@comp, op, $comp);
            op.extend([I, I, I]);
            op
        }};

        // C-instruction (without jump)
        ($dest:tt = $comp:tt) => {
            op!($dest = $comp ; null)
        };

        // C-instruction (without destination)
        ($comp:tt ; $jump:tt) => {{
            op!(null =  $comp ; $jump)
        }};

        // C-instruction (bare computation)
        ($comp:tt) => {{
            op!(null = $comp ; null)
        }};

        (@jump, $bits:expr, null) => { $bits.extend([O, O, O]); };
        (@jump, $bits:expr, JGT) => { $bits.extend([I, O, O]); };
        (@jump, $bits:expr, JEQ) => { $bits.extend([O, I, O]); };
        (@jump, $bits:expr, JGE) => { $bits.extend([I, I, O]); };
        (@jump, $bits:expr, JLT) => { $bits.extend([O, O, I]); };
        (@jump, $bits:expr, JNE) => { $bits.extend([I, O, I]); };
        (@jump, $bits:expr, JLE) => { $bits.extend([O, I, I]); };
        (@jump, $bits:expr, JMP) => { $bits.extend([I, I, I]); };

        (@dest, $bits:expr, null) => { $bits.extend([O, O, O]); };
        (@dest, $bits:expr,    M) => { $bits.extend([I, O, O]); };
        (@dest, $bits:expr,    D) => { $bits.extend([O, I, O]); };
        (@dest, $bits:expr,   DM) => { $bits.extend([O, I, I]); };
        (@dest, $bits:expr,    A) => { $bits.extend([O, O, I]); };
        (@dest, $bits:expr,   AM) => { $bits.extend([I, O, I]); };
        (@dest, $bits:expr,   AD) => { $bits.extend([O, I, I]); };
        (@dest, $bits:expr,  ADM) => { $bits.extend([I, I, I]); };

        (@comp, $bits:expr, (0)) => { $bits.extend([O, I, O, I, O, I, O]); };
        (@comp, $bits:expr, (1)) => { $bits.extend([I, I, I, I, I, I, O]); };
        (@comp, $bits:expr, (-1)) => { $bits.extend([O, I, O, I, I, I, O]); };
        (@comp, $bits:expr, (D)) => { $bits.extend([O, O, I, I, O, O, O]); };
        (@comp, $bits:expr, (A)) => { $bits.extend([O, O, O, O, I, I, O]); };
        (@comp, $bits:expr, (M)) => { $bits.extend([O, O, O, O, I, I, I]); };
        (@comp, $bits:expr, (!D)) => { $bits.extend([I, O, I, I, O, O, O]); };
        (@comp, $bits:expr, (!A)) => { $bits.extend([I, O, O, O, I, I, O]); };
        (@comp, $bits:expr, (!M)) => { $bits.extend([I, O, O, O, I, I, I]); };
        (@comp, $bits:expr, (-D)) => { $bits.extend([I, I, I, I, O, O, O]); };
        (@comp, $bits:expr, (-A)) => { $bits.extend([I, I, O, O, I, I, O]); };
        (@comp, $bits:expr, (-M)) => { $bits.extend([I, I, O, O, I, I, I]); };
        (@comp, $bits:expr, (D+1)) => { $bits.extend([I, I, I, I, I, O, O]); };
        (@comp, $bits:expr, (A+1)) => { $bits.extend([I, I, I, O, I, I, O]); };
        (@comp, $bits:expr, (M+1)) => { $bits.extend([I, I, I, O, I, I, I]); };
        (@comp, $bits:expr, (D-1)) => { $bits.extend([O, I, I, I, O, O, O]); };
        (@comp, $bits:expr, (A-1)) => { $bits.extend([O, I, O, O, I, I, O]); };
        (@comp, $bits:expr, (M-1)) => { $bits.extend([O, I, O, O, I, I, I]); };
        (@comp, $bits:expr, (D+A)) => { $bits.extend([O, I, O, O, O, O, O]); };
        (@comp, $bits:expr, (A+D)) => { $bits.extend([O, I, O, O, O, O, O]); };
        (@comp, $bits:expr, (D+M)) => { $bits.extend([O, I, O, O, O, O, I]); };
        (@comp, $bits:expr, (M+D)) => { $bits.extend([O, I, O, O, O, O, I]); };
        (@comp, $bits:expr, (D-A)) => { $bits.extend([I, I, O, O, I, O, O]); };
        (@comp, $bits:expr, (D-M)) => { $bits.extend([I, I, O, O, I, O, I]); };
        (@comp, $bits:expr, (A-D)) => { $bits.extend([I, I, I, O, O, O, O]); };
        (@comp, $bits:expr, (M-D)) => { $bits.extend([I, I, I, O, O, O, I]); };
        (@comp, $bits:expr, (D&A)) => { $bits.extend([O, O, O, O, O, O, O]); };
        (@comp, $bits:expr, (A&D)) => { $bits.extend([O, O, O, O, O, O, O]); };
        (@comp, $bits:expr, (D&M)) => { $bits.extend([O, O, O, O, O, O, I]); };
        (@comp, $bits:expr, (M&D)) => { $bits.extend([O, O, O, O, O, O, I]); };
        (@comp, $bits:expr, (D|M)) => { $bits.extend([I, O, I, O, I, O, I]); };
        (@comp, $bits:expr, (M|D)) => { $bits.extend([I, O, I, O, I, O, I]); };
    }

    const Z15: [Bit; 15] = [O; 15];
    const ONE15: [Bit; 15] = [I, O, O, O, O, O, O, O, O, O, O, O, O, O, O];
    const NEG15: [Bit; 15] = [I, I, I, I, I, I, I, I, I, I, I, I, I, I, I];
    const Z16: [Bit; 16] = [O; 16];
    const ONE16: [Bit; 16] = [I, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O];
    const TWO16: [Bit; 16] = [O, I, O, O, O, O, O, O, O, O, O, O, O, O, O, O];
    const THREE16: [Bit; 16] = [I, I, O, O, O, O, O, O, O, O, O, O, O, O, O, O];
    const NEG16: [Bit; 16] = [I, I, I, I, I, I, I, I, I, I, I, I, I, I, I, I];
    const NTWO16: [Bit; 16] = [O, I, I, I, I, I, I, I, I, I, I, I, I, I, I, I];
    const AA16: [Bit; 16] = [I, O, O, O, O, O, I, O, I, O, O, O, O, O, I, O]; // ASCII AA
    const DD16: [Bit; 16] = [O, O, I, O, O, O, I, O, O, O, I, O, O, O, I, O]; // ASCII DD
    const MM16: [Bit; 16] = [I, O, I, I, O, O, I, O, I, O, I, I, O, O, I, O]; // ASCII MM
    const LSB16: [Bit; 16] = [O, O, O, O, O, O, O, O, I, I, I, I, I, I, I, I];
    const MSB16: [Bit; 16] = [I, I, I, I, I, I, I, I, O, O, O, O, O, O, O, O];
    const MAX_SIGNED_16: [Bit; 16] = [I, I, I, I, I, I, I, I, I, I, I, I, I, I, I, O];
    const CHAR_A16: [Bit; 16] = [I, O, O, O, O, O, I, O, O, O, O, O, O, O, O, O];

    const ADDR_RAM_FIRST: [Bit; 15] = Z15;
    const ADDR_RAM_LAST: [Bit; 15] = [I, I, I, I, I, I, I, I, I, I, I, I, I, I, O];
    const ADDR_SCREEN_FIRST: [Bit; 15] = [O, O, O, O, O, O, O, O, O, O, O, O, O, O, I];
    const ADDR_SCREEN_2ND: [Bit; 15] = [I, O, O, O, O, O, O, O, O, O, O, O, O, O, I];
    const ADDR_KEYBOARD: [Bit; 15] = [O, O, O, O, O, O, O, O, O, O, O, O, O, I, I];

    fn create_computer(sb: &mut SystemBuilder<Bit>, name: &str, reset: &Wire<Bit>) -> Computer {
        let instructions = [
            // i = 1
            bus_as_number(&op!(@101)),
            bus_as_number(&op!(M = (1))),
            // sum = 0
            bus_as_number(&op!(@100)),
            bus_as_number(&op!(M = (0))),
            // (LOOP) = 4
            // if (i > R0) goto STOP
            bus_as_number(&op!(@101)),
            bus_as_number(&op!(D = (M))),
            bus_as_number(&op!(@0)),
            bus_as_number(&op!(D = (D - M))),
            bus_as_number(&op!(@20)),
            bus_as_number(&op!((D);JGT)),
            // sum += i
            bus_as_number(&op!(@100)),
            bus_as_number(&op!(D = (M))),
            bus_as_number(&op!(@101)),
            bus_as_number(&op!(D = (D + M))),
            bus_as_number(&op!(@100)),
            bus_as_number(&op!(M = (D))),
            bus_as_number(&op!(@101)),
            bus_as_number(&op!(M = (M + 1))),
            // goto LOOP
            bus_as_number(&op!(@4)),
            bus_as_number(&op!((0);JMP)),
            // (STOP) = 20
            // R1 = sum
            bus_as_number(&op!(@100)),
            bus_as_number(&op!(D = (M))),
            bus_as_number(&op!(@1)),
            bus_as_number(&op!(M = (D))),
            // (END) = 24
            bus_as_number(&op!(@24)),
            bus_as_number(&op!((0);JMP)),
        ];

        let mut rom_data = vec![0; 32738];
        for (r, i) in rom_data.iter_mut().zip(instructions) {
            *r = i;
        }

        Computer::new(sb, name, reset, Arc::new(rom_data))
    }

    #[test]
    fn computer() {
        const VAR_SUM: usize = 100;
        const VAR_I: usize = 101;
        system! {
            sys, comp
            wires { reset }
            buses { }
            gates {
                create_computer("RAM", reset);
            }
            body {
                comp.ram.borrow_mut()[0] = 10;

                // reset
                assert_cycle!(sys, reset=I =>);
                assert_sim!(sys, reset=O =>);

                // simulate a sufficient number of cycles
                for _ in 1..=175 {
                    sys.cycle();
                    sys.simulate();
                }

                assert_eq!(comp.ram.borrow()[VAR_I], 11);
                assert_eq!(comp.ram.borrow()[VAR_SUM], 55);
                assert_eq!(comp.ram.borrow()[1], 55);
            }
        }
    }

    #[test]
    fn memory() {
        let screen = Arc::new(RefCell::new(vec![0; 0x2000]));
        let keyboard = Arc::new(RefCell::new(vec![65; 1]));

        let scr: Arc<dyn MemoryDevice> = screen.clone();
        let kbd: Arc<dyn MemoryDevice> = keyboard.clone();

        system! {
            sys
            wires { load }
            buses { inval[16], addr[15], outval[16] }
            gates {
                make_memory("RAM", load, inval, addr, outval, scr, kbd);
            }
            body {
                // write to first and last RAM addresses ...
                assert_cycle!(sys, addr=&ADDR_RAM_FIRST, inval=&AA16, load=I =>);
                assert_cycle!(sys, addr=&ADDR_RAM_LAST, inval=&MM16, load=I =>);

                // ... then check if the values can be read back
                assert_cycle!(sys, addr=&ADDR_RAM_FIRST, load=O => outval=&AA16);
                assert_cycle!(sys, addr=&ADDR_RAM_LAST, load=O => outval=&MM16);

                // writing to RAM does not change SCREEN
                assert_cycle!(sys, addr=&ADDR_RAM_FIRST, inval=&AA16, load=I =>);
                assert_cycle!(sys, addr=&ADDR_SCREEN_FIRST, load=O => outval=&Z16);

                // write to and read from screen, and check if the value is in the storage
                assert_cycle!(sys, addr=&ADDR_SCREEN_FIRST, inval=&MSB16, load=I =>);
                assert_cycle!(sys, addr=&ADDR_SCREEN_2ND, inval=&LSB16, load=I =>);
                assert_cycle!(sys, addr=&ADDR_SCREEN_FIRST, load=O => outval=&MSB16);
                assert_cycle!(sys, addr=&ADDR_SCREEN_2ND, load=O => outval=&LSB16);
                assert_eq!(screen.borrow()[0], 0x00FF);
                assert_eq!(screen.borrow()[1], 0xFF00);

                // read a character from the keyboard
                assert_cycle!(sys, addr=&ADDR_KEYBOARD, load=O => outval=&CHAR_A16);

                // writes to the keyboard are ignored
                assert_cycle!(sys, addr=&ADDR_KEYBOARD, inval=&Z16, load=I =>);
                assert_cycle!(sys, addr=&ADDR_KEYBOARD, load=O => outval=&CHAR_A16);
            }
        }
    }

    #[test]
    fn test_cpu() {
        system! {
            sys, cpu
            wires { reset, write }
            buses { instruction[16], inval[16], outval[16], outaddr[15], pc[15] }
            gates {
                make_cpu("CPU", instruction, inval, reset, outval, outaddr, write, pc);
            }
            body {
                // after resetting, the program counter starts at zero, then increases by 1
                assert_cycle!(sys, reset=I => write=O, pc=Z15);
                assert_cycle!(sys, reset=O => write=O, pc=ONE15);

                // an A instruction sets the output address but not the program counter
                assert_cycle!(sys,
                    instruction=&op!(@0b111100001100101) =>
                         outaddr=[I, O, I, O, O, I, I, O, O, O, O, I, I, I, I],
                              pc=[O, I, O, O, O, O, O, O, O, O, O, O, O, O, O],
                           write=O);

                // an A instruction does not set any other register
                assert_cycle!(sys, instruction=&op!(D=(0)) =>);
                assert_cycle!(sys, instruction=&op!(@0b111111111111111) => );
                assert_eq!(cpu.a_val.value(), MAX_SIGNED_16);
                assert_eq!(cpu.d_val.value(), Z16);

                // compute 1 and store it in RAM[A]
                assert_cycle!(sys,
                    instruction=&op!(M=(1)) =>
                    write=I, outval=ONE16);

                // compute 0 and store it in RAM[A]
                assert_cycle!(sys,
                    instruction=&op!(M=(0)) =>
                    write=I, outval=Z16);

                // compute -1 and store it in A
                assert_sim!(sys, instruction=&op!(A=(-1)) => write=O);
                assert_eq!(cpu.a_load.value(), I);
                assert_eq!(cpu.a_src.value(), NEG16);

                // compute 1 and store it in A
                assert_sim!(sys, instruction=&op!(A=(1)) => write=O);
                assert_eq!(cpu.a_load.value(), I);
                assert_eq!(cpu.a_src.value(), ONE16);

                // compute -1 and store it in D
                assert_cycle!(sys, instruction=&op!(D=(-1)) => write=O, outval=NEG16);
                assert_eq!(cpu.d_load.value(), I);
                assert_eq!(cpu.d_val.value(), NEG16);

                // compute 1 and store it in D
                assert_cycle!(sys, instruction=&op!(D=(1)) => write=O, outval=ONE16);
                assert_eq!(cpu.d_load.value(), I);
                assert_eq!(cpu.d_val.value(), ONE16);

                // compute -1 and store it in A, D, and RAM[A]
                assert_cycle!(sys,
                    instruction=&op!(ADM=(-1)) =>
                    outaddr=NEG15, write=I, outval=NEG16);
                assert_eq!(cpu.a_load.value(), I);
                assert_eq!(cpu.d_load.value(), I);
                assert_eq!(cpu.a_src.value(), NEG16);
                assert_eq!(cpu.d_val.value(), NEG16);

                // compute 1 and don't store it
                assert_sim!(sys, instruction=&op!((1)) => write=O, outval=ONE16);
                assert_eq!(cpu.a_load.value(), O);
                assert_eq!(cpu.d_load.value(), O);

                // get value of memory input
                assert_sim!(sys, instruction=&op!((M)), inval=&MM16 => outval=MM16);

                // get value of A register
                assert_cycle!(sys, instruction=&AA16 =>);  // A = "AA"
                assert_sim!(sys, instruction=&op!((A)) => outval=AA16);

                // get value of D register
                assert_cycle!(sys, instruction=&op!(D=(M)), inval=&DD16 =>);  // D = "DD"
                assert_sim!(sys, instruction=&op!((D)) => outval=DD16);

                // compute !D
                assert_cycle!(sys, instruction=&op!(D=(0)) =>);
                assert_sim!(sys, instruction=&op!((!D)) => outval=NEG16);

                // compute !A
                assert_cycle!(sys, instruction=&op!(A=(0)) =>);
                assert_sim!(sys, instruction=&op!((!A)) => outval=NEG16);

                // compute !M
                assert_sim!(sys, instruction=&op!((!M)), inval=&Z16 => outval=NEG16);

                // compute -D
                assert_cycle!(sys, instruction=&op!(D=(-1)) =>);
                assert_sim!(sys, instruction=&op!((-D)) => outval=ONE16);

                // compute -A
                assert_cycle!(sys, instruction=&op!(A=(-1)) =>);
                assert_sim!(sys, instruction=&op!((-A)) => outval=ONE16);

                // compute -M
                assert_sim!(sys, instruction=&op!((-M)), inval=&ONE16 => outval=NEG16);

                // compute D+1
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);
                assert_sim!(sys, instruction=&op!((D+1)) => outval=TWO16);

                // compute A+1
                assert_cycle!(sys, instruction=&op!(A=(0)) =>);
                assert_sim!(sys, instruction=&op!((A+1)) => outval=ONE16);

                // compute M+1
                assert_sim!(sys, instruction=&op!((M+1)), inval=&NEG16 => outval=Z16);

                // compute D-1
                assert_cycle!(sys, instruction=&op!(D=(-1)) =>);
                assert_sim!(sys, instruction=&op!((D-1)) => outval=NTWO16);

                // compute A-1
                assert_cycle!(sys, instruction=&op!(A=(0)) =>);
                assert_sim!(sys, instruction=&op!((A-1)) => outval=NEG16);

                // compute M-1
                assert_sim!(sys, instruction=&op!((M-1)), inval=&ONE16 => outval=Z16);

                // compute A + D
                assert_cycle!(sys, instruction=&op!(A=(1)) =>);  // A = 1
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);  // D = 1
                assert_sim!(sys, instruction=&op!((D+A)) => outval=TWO16);

                // compute M + D
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);  // D = 1
                assert_sim!(sys, instruction=&op!((D+M)), inval=&ONE16 => outval=TWO16);

                // compute D - A
                assert_cycle!(sys, instruction=&op!(A=(1)) =>);  // A = 1
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);  // D = 1
                assert_sim!(sys, instruction=&op!((D-A)) => outval=Z16);

                // compute D - M
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);  // D = 1
                assert_sim!(sys, instruction=&op!((D-M)), inval=&ONE16 => outval=Z16);

                // compute A - D
                assert_cycle!(sys, instruction=&op!(A=(1)) =>);  // A = 1
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);  // D = 1
                assert_sim!(sys, instruction=&op!((A-D)) => outval=Z16);

                // compute M - D
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);  // D = 1
                assert_sim!(sys, instruction=&op!((M-D)), inval=&ONE16 => outval=Z16);

                // compute M & D
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);  // D = 1
                assert_sim!(sys, instruction=&op!((D&M)), inval=&THREE16 => outval=ONE16);

                // compute M | D
                assert_cycle!(sys, instruction=&op!(D=(1)) =>);  // D = 1
                assert_sim!(sys, instruction=&op!((D|M)), inval=&TWO16 => outval=THREE16);

                // an A instruction sets the output address (and the A register)
                assert_cycle!(sys,
                    //instruction=&op!(@0b111100001100101) =>
                    instruction=&op!(@0b111100001100101) =>
                         outaddr=[I, O, I, O, O, I, I, O, O, O, O, I, I, I, I], write=O);
                // an unconditional jump sets the program counter to the value of A
                assert_cycle!(sys,
                    instruction=&op!((0);JMP) =>
                         outaddr=[I, O, I, O, O, I, I, O, O, O, O, I, I, I, I],
                            pc = [I, O, I, O, O, I, I, O, O, O, O, I, I, I, I],
                         write=O);
                assert_eq!(cpu.jump.value(), I);

                // jump if M > 0
                assert_sim!(sys, instruction=&op!((M);JGT), inval=&NEG16 =>);
                assert_eq!(cpu.jump.value(), O);
                assert_sim!(sys, instruction=&op!((M);JGT), inval=&Z16 =>);
                assert_eq!(cpu.jump.value(), O);
                assert_sim!(sys, instruction=&op!((M);JGT), inval=&ONE16 =>);
                assert_eq!(cpu.jump.value(), I);

                // jump if M = 0
                assert_sim!(sys, instruction=&op!((M);JEQ), inval=&NEG16 =>);
                assert_eq!(cpu.jump.value(), O);
                assert_sim!(sys, instruction=&op!((M);JEQ), inval=&Z16 =>);
                assert_eq!(cpu.jump.value(), I);
                assert_sim!(sys, instruction=&op!((M);JEQ), inval=&ONE16 =>);
                assert_eq!(cpu.jump.value(), O);

                // jump if M >= 0
                assert_sim!(sys, instruction=&op!((M);JGE), inval=&NEG16 =>);
                assert_eq!(cpu.jump.value(), O);
                assert_sim!(sys, instruction=&op!((M);JGE), inval=&Z16 =>);
                assert_eq!(cpu.jump.value(), I);
                assert_sim!(sys, instruction=&op!((M);JGE), inval=&ONE16 =>);
                assert_eq!(cpu.jump.value(), I);

                // jump if M < 0
                assert_sim!(sys, instruction=&op!((M);JLT), inval=&NEG16 =>);
                assert_eq!(cpu.jump.value(), I);
                assert_sim!(sys, instruction=&op!((M);JLT), inval=&Z16 =>);
                assert_eq!(cpu.jump.value(), O);
                assert_sim!(sys, instruction=&op!((M);JLT), inval=&ONE16 =>);
                assert_eq!(cpu.jump.value(), O);

                // jump if M != 0
                assert_sim!(sys, instruction=&op!((M);JNE), inval=&NEG16 =>);
                assert_eq!(cpu.jump.value(), I);
                assert_sim!(sys, instruction=&op!((M);JNE), inval=&Z16 =>);
                assert_eq!(cpu.jump.value(), O);
                assert_sim!(sys, instruction=&op!((M);JNE), inval=&ONE16 =>);
                assert_eq!(cpu.jump.value(), I);

                // jump if M <= 0
                assert_sim!(sys, instruction=&op!((M);JLE), inval=&NEG16 =>);
                assert_eq!(cpu.jump.value(), I);
                assert_sim!(sys, instruction=&op!((M);JLE), inval=&Z16 =>);
                assert_eq!(cpu.jump.value(), I);
                assert_sim!(sys, instruction=&op!((M);JLE), inval=&ONE16 =>);
                assert_eq!(cpu.jump.value(), O);
            }
        }
    }
}
