use crate::chapter01_boolean_logic::Bit::{I, O};
use crate::chapter01_boolean_logic::{make_and, make_identity, make_not, make_or_reduce, Bit};
use crate::chapter02_boolean_arithmetic::make_alu;
use crate::chapter03_memory::{make_counter, make_register};
use crate::hardware::{make_constant, ClockHandler, MemoryCell, SystemBuilder, Wire};
use std::fmt::format;

pub fn make_cpu(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    instruction: &[Wire<Bit>],
    inmem: &[Wire<Bit>],
    reset: &Wire<Bit>,
    outmem: &[Wire<Bit>],
    outaddr: &[Wire<Bit>],
    write: &Wire<Bit>,
    pc: &[Wire<Bit>],
) {
    assert_eq!(instruction.len(), 16);
    assert_eq!(inmem.len(), 16);
    assert_eq!(outmem.len(), 16);
    assert_eq!(outaddr.len(), 15);
    assert_eq!(pc.len(), 15);
    let name = name.into();

    // temporary constants
    let_wires!(zero, one);
    make_constant(sb, "zero", O, &zero);
    make_constant(sb, "one", I, &one);

    let jjj = &instruction[0..3];
    let ddd = &instruction[3..6];
    let no = &instruction[6];
    let f = &instruction[7];
    let ny = &instruction[8];
    let zy = &instruction[9];
    let nx = &instruction[10];
    let zx = &instruction[11];
    let a = &instruction[12];
    let c_instruction = &instruction[15];

    // write output?
    make_and(sb, format!("{}.write", name), c_instruction, &ddd[0], write);

    // A register
    let_wires!(a_load, a_msb);
    let mut a_val = outaddr.to_vec();
    a_val.push(a_msb);
    make_not(
        sb,
        format!("{}.a-load", name),
        instruction.last().unwrap(),
        &a_load,
    );
    make_register(
        sb,
        format!("{}.RA", name),
        instruction, /*tmp*/
        &a_load,
        &a_val,
    );

    // jump logic (todo: conditional jumps)
    let_wires!(jump);
    make_or_reduce(sb, format!("{}.jmp", name), jjj, &jump);

    // program counter
    make_counter(sb, format!("{}.pc", name), &jump, &one, reset, outaddr, pc);

    let_buses!(d_val[16]);

    // alu
    let_wires!(zr, ng);
    make_alu(
        sb,
        format!("{}.alu", name),
        &d_val,
        &a_val, /*tmp*/
        zx,
        nx,
        zy,
        ny,
        f,
        no,
        outmem,
        &zr,
        &ng,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hardware::{BusApi, ClockHandler};
    use Bit::{I, O};

    const Z15: [Bit; 15] = [O; 15];
    const ONE15: [Bit; 15] = [I, O, O, O, O, O, O, O, O, O, O, O, O, O, O];
    const Z16: [Bit; 16] = [O; 16];
    const ONE16: [Bit; 16] = [I, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O];

    #[test]
    fn test_cpu() {
        system! {
            sys
            wires { reset, write }
            buses { instruction[16], inmem[16], outmem[16], outaddr[15], pc[15] }
            gates {
                make_cpu("CPU", instruction, inmem, reset, outmem, outaddr, write, pc);
            }
            body {
                // after resetting, the program counter starts at zero, then increases by 1
                assert_cycle!(sys, reset=I => write=O, pc=Z15);
                assert_cycle!(sys, reset=O => write=O, pc=ONE15);

                // compute 1 and store it in RAM[A=0]
                assert_cycle!(sys,
                    instruction=&[O, O, O, I, O, O, I, I, I, I, I, I, O, I, I, I] =>
                         outaddr=Z15, write=I, outmem=ONE16);

                // compute 0 and store it in RAM[A=0]
                assert_cycle!(sys,
                    instruction=&[O, O, O, I, O, O, O, I, O, I, O, I, O, I, I, I] =>
                         outaddr=Z15, write=I, outmem=Z16);

                todo!("test more alu instructions");

                // an A instruction sets the output address (and the A register)
                assert_cycle!(sys,
                    instruction=&[I, I, I, I, I, I, I, I, I, I, I, I, I, I, I, O] =>
                         outaddr=[I, I, I, I, I, I, I, I, I, I, I, I, I, I, I], write=O);
                // an unconditional jump sets the program counter to the value of A
                assert_cycle!(sys,
                    instruction=&[I, I, I, O, O, O, O, I, O, I, O, I, O, I, I, I] =>
                         outaddr=[I, I, I, I, I, I, I, I, I, I, I, I, I, I, I],
                            pc = [I, I, I, I, I, I, I, I, I, I, I, I, I, I, I],
                         write=O);

                todo!()
            }
        }
    }
}
