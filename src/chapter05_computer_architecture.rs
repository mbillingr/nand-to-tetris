use crate::chapter01_boolean_logic::Bit;
use crate::chapter01_boolean_logic::Bit::{I, O};
use crate::chapter03_memory::make_counter;
use crate::hardware::{make_constant, ClockHandler, MemoryCell, SystemBuilder, Wire};

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

    let_buses!(a_val[15]);

    make_counter(sb, format!("{}.pc", name), &zero, &one, reset, &a_val, pc);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hardware::{BusApi, ClockHandler};
    use Bit::{I, O};

    const Z15: [Bit; 15] = [O; 15];
    const ONE15: [Bit; 15] = [I, O, O, O, O, O, O, O, O, O, O, O, O, O, O];
    const Z16: [Bit; 16] = [O; 16];

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
                assert_cycle!(sys, instruction=&Z16, inmem=&Z16, reset=I => write=O, pc=Z15);
                assert_cycle!(sys, instruction=&Z16, inmem=&Z16, reset=O => write=O, pc=ONE15);
                todo!()
            }
        }
    }
}
