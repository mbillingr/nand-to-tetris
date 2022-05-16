use crate::chapter01_boolean_logic::{make_mux, Bit};
use crate::hardware::{ClockHandler, MemoryCell, SystemBuilder, Wire};
use std::sync::Arc;
use Bit::O;

pub fn make_dflipflop(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<Box<str>>,
    x: &Wire<Bit>,
    y: &Wire<Bit>,
) {
    let name = name.into();

    let memory_cell = Arc::new(MemoryCell::new(O));

    let mc_set = memory_cell.clone();
    sb.add_device(format!("{}.set", name), &[&x], &[], move |inp, _| {
        mc_set.load(inp[0])
    });

    let mc_get = memory_cell.clone();
    sb.add_device(format!("{}.get", name), &[], &[&y], move |_, out| {
        out.push(mc_get.fetch())
    });

    let mc_hnd: Arc<dyn ClockHandler> = memory_cell;
    sb.add_clock_handler(mc_hnd);
}

pub fn make_bit_register(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    i: &Wire<Bit>,
    load: &Wire<Bit>,
    o: &Wire<Bit>,
) {
    let name = name.into();
    let_wires!(data);
    make_mux(sb, format!("{}.mux", name), o, i, load, &data);
    make_dflipflop(sb, format!("{}.dff", name), &data, o);
}

pub fn make_register(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    i: &[Wire<Bit>],
    load: &Wire<Bit>,
    o: &[Wire<Bit>],
) {
    assert_eq!(i.len(), o.len());
    let name = name.into();
    for (k, (x, y)) in i.iter().zip(o).enumerate() {
        make_bit_register(sb, format!("{}[{}]", name, k), x, load, y);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hardware::{BusApi, ClockHandler};
    use Bit::{I, O};

    #[test]
    fn test_bit_register() {
        system! {
            sys
            wires { i, load, o }
            gates {
                make_bit_register("BIT", i, load, o);
            }
            body {
                assert_cycle!(sys, i=O, load=O => o=O);
                assert_cycle!(sys, i=I, load=O => o=O);
                assert_cycle!(sys, i=O, load=I => o=O);
                assert_cycle!(sys, i=I, load=I => o=I);
                assert_cycle!(sys, i=O, load=O => o=I);
                assert_cycle!(sys, i=I, load=O => o=I);
                assert_cycle!(sys, i=I, load=I => o=I);
                assert_cycle!(sys, i=O, load=I => o=O);
            }
        }
    }

    #[test]
    fn test_register() {
        system! {
            sys
            wires { load }
            buses { i[2], o[2] }
            gates {
                make_register("REG", i, load, o);
            }
            body {
                assert_cycle!(sys, i=&[O, I], load=O => o=[O, O]);
                assert_cycle!(sys, i=&[O, I], load=I => o=[O, I]);
                assert_cycle!(sys, i=&[I, O], load=I => o=[I, O]);
                assert_cycle!(sys, i=&[O, O], load=O => o=[I, O]);
                assert_cycle!(sys, i=&[I, I], load=I => o=[I, I]);
            }
        }
    }
}
