use crate::chapter01_boolean_logic::Bit::{I, O};
use crate::chapter01_boolean_logic::{
    make_and, make_identity, make_mux_bus, make_not, make_or, make_or_reduce, Bit,
};
use crate::chapter02_boolean_arithmetic::make_alu;
use crate::chapter03_memory::{make_counter, make_register};
use crate::hardware::{make_constant, ClockHandler, MemoryCell, SystemBuilder, Wire};
use std::fmt::format;

pub struct Cpu {
    dst_a: Wire<Bit>,
    a_load: Wire<Bit>,
    a_src: Vec<Wire<Bit>>,
    a_val: Vec<Wire<Bit>>,
    d_load: Wire<Bit>,
    d_src: Vec<Wire<Bit>>,
    d_val: Vec<Wire<Bit>>,
    jump: Wire<Bit>,
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
    let d_load = dst_d;
    let d_src = outval;
    let_buses!(d_val[16]);
    make_register(sb, format!("{}.RD", name), d_src, d_load, &d_val);

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

    // jump logic (todo: conditional jumps)
    let_wires!(jn, jp, jz, jump);
    make_and(sb, format!("{}.jmp+", name), jmp_pos, &ps, &jp);
    make_and(sb, format!("{}.jmp0", name), jmp_zero, &zr, &jz);
    make_and(sb, format!("{}.jmp-", name), jmp_neg, &ng, &jn);
    make_or_reduce(sb, format!("{}.jmp", name), &[jp, jz, jn], &jump);

    // program counter
    make_counter(sb, format!("{}.pc", name), &jump, &one, reset, outaddr, pc);

    Cpu {
        dst_a: dst_a.clone(),
        a_load,
        a_src,
        a_val,
        d_load: d_load.clone(),
        d_src: d_src.to_vec(),
        d_val,
        jump,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hardware::{BusApi, ClockHandler};
    use Bit::{I, O};

    const Z15: [Bit; 15] = [O; 15];
    const ONE15: [Bit; 15] = [I, O, O, O, O, O, O, O, O, O, O, O, O, O, O];
    const TWO15: [Bit; 15] = [O, I, O, O, O, O, O, O, O, O, O, O, O, O, O];
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

                // an A instruction sets the output address (and the A register)

                // compute 1 and store it in RAM[A=0]
                assert_cycle!(sys,
                    instruction=&op!(M=(1)) =>
                    outaddr=Z15, write=I, outval=ONE16);

                // compute 0 and store it in RAM[A=0]
                assert_cycle!(sys,
                    instruction=&op!(M=(0)) =>
                    outaddr=Z15, write=I, outval=Z16);

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
