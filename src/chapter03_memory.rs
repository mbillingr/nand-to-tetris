use crate::chapter01_boolean_logic::{
    make_and, make_and_bit_bus, make_mux, make_mux_bus, make_not, make_or_reduce, Bit,
};
use crate::chapter02_boolean_arithmetic::make_incrementer;
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

pub fn make_ramn(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    n: usize,
    addr: &[Wire<Bit>],
    x: &[Wire<Bit>],
    load: &Wire<Bit>,
    y: &[Wire<Bit>],
) {
    assert!(n > 0);
    assert_eq!(x.len(), y.len());
    let name = name.into();
    let width = x.len();

    let sel = &addr[n - 1];
    let_wires!(nsel, load0, load1);
    make_not(sb, format!("{}.nsel", name), sel, &nsel);
    make_and(sb, format!("{}.load0", name), &nsel, load, &load0);
    make_and(sb, format!("{}.load1", name), sel, load, &load1);
    let_buses!(y0[width], y1[width]);
    make_mux_bus(sb, format!("{}.output", name), &y0, &y1, sel, y);

    if n == 1 {
        make_register(sb, format!("{}.word0", name), x, &load0, &y0);
        make_register(sb, format!("{}.word1", name), x, &load1, &y1);
    } else {
        make_ramn(sb, format!("{}.word0", name), n - 1, addr, x, &load0, &y0);
        make_ramn(sb, format!("{}.word1", name), n - 1, addr, x, &load1, &y1);
    }
}

pub fn make_counter(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    load: &Wire<Bit>,
    inc: &Wire<Bit>,
    reset: &Wire<Bit>,
    inval: &[Wire<Bit>],
    out: &[Wire<Bit>],
) {
    assert_eq!(inval.len(), out.len());
    let width = out.len();
    let name = name.into();

    let_wires!(set);
    make_or_reduce(
        sb,
        format!("{}.set", name),
        &[load.clone(), reset.clone(), inc.clone()],
        &set,
    );

    let_wires!(not_reset);
    make_not(sb, format!("{}.not-reset", name), reset, &not_reset);

    let_buses!(incr[width]);
    make_incrementer(sb, format!("{}.inc", name), out, &incr);

    let_buses!(value[width]);
    make_mux_bus(sb, format!("{}.old/new", name), &incr, inval, load, &value);

    let_buses!(setval[width]);
    make_and_bit_bus(sb, format!("{}.value", name), &not_reset, &value, &setval);

    make_register(sb, format!("{}.state", name), &setval, &set, &out);
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

    fn make_ram2(
        sb: &mut SystemBuilder<Bit>,
        name: impl Into<String>,
        addr: &[Wire<Bit>],
        x: &[Wire<Bit>],
        load: &Wire<Bit>,
        y: &[Wire<Bit>],
    ) {
        make_ramn(sb, name, 1, addr, x, load, y)
    }

    #[test]
    fn test_ram2() {
        system! {
            sys
            wires { load }
            buses { i[2], o[2], addr[1] }
            gates {
                make_ram2("REG", addr, i, load, o);
            }
            body {
                assert_cycle!(sys, addr=&[O], i=&[O, I], load=O => o=[O, O]);
                assert_cycle!(sys, addr=&[I], i=&[O, I], load=O => o=[O, O]);
                assert_cycle!(sys, addr=&[O], i=&[I, I], load=I => o=[I, I]);
                assert_cycle!(sys, addr=&[I], i=&[O, I], load=I => o=[O, I]);
                assert_cycle!(sys, addr=&[O], i=&[O, O], load=O => o=[I, I]);
                assert_cycle!(sys, addr=&[I], i=&[O, O], load=O => o=[O, I]);
            }
        }
    }

    fn make_ram8(
        sb: &mut SystemBuilder<Bit>,
        name: impl Into<String>,
        addr: &[Wire<Bit>],
        x: &[Wire<Bit>],
        load: &Wire<Bit>,
        y: &[Wire<Bit>],
    ) {
        make_ramn(sb, name, 3, addr, x, load, y)
    }

    #[test]
    fn test_ram8() {
        system! {
            sys
            wires { load }
            buses { i[2], o[2], addr[3] }
            gates {
                make_ram8("REG", addr, i, load, o);
            }
            body {
                assert_cycle!(sys, addr=&[O, O, O], i=&[O, I], load=O => o=[O, O]);
                assert_cycle!(sys, addr=&[O, I, O], i=&[O, I], load=O => o=[O, O]);
                assert_cycle!(sys, addr=&[O, O, O], i=&[I, I], load=I => o=[I, I]);
                assert_cycle!(sys, addr=&[O, I, O], i=&[O, I], load=I => o=[O, I]);
                assert_cycle!(sys, addr=&[O, O, O], i=&[O, O], load=O => o=[I, I]);
                assert_cycle!(sys, addr=&[O, I, O], i=&[O, O], load=O => o=[O, I]);
            }
        }
    }

    #[test]
    fn test_counter() {
        system! {
            sys
            wires { load, inc, reset }
            buses { i[2], o[2] }
            gates {
                make_counter("REG", load, inc, reset, i, o);
            }
            body {
                assert_cycle!(sys, i=&[O, O], load=O, inc=O, reset=I => o=[O, O]);  // start resetted
                assert_cycle!(sys, i=&[O, O], load=O, inc=I, reset=O => o=[I, O]);  // normal counting
                assert_cycle!(sys, i=&[O, O], load=O, inc=I, reset=O => o=[O, I]);  // ...
                assert_cycle!(sys, i=&[O, O], load=O, inc=I, reset=O => o=[I, I]);  // ...
                assert_cycle!(sys, i=&[O, O], load=O, inc=I, reset=O => o=[O, O]);  // ...
                assert_cycle!(sys, i=&[I, I], load=I, inc=O, reset=O => o=[I, I]);  // load a value
                assert_cycle!(sys, i=&[O, I], load=I, inc=O, reset=O => o=[O, I]);  // load another value
                assert_cycle!(sys, i=&[O, O], load=O, inc=O, reset=O => o=[O, I]);  // do nothing
                assert_cycle!(sys, i=&[I, I], load=I, inc=I, reset=I => o=[O, O]);  // reset has highest precedence
                assert_cycle!(sys, i=&[I, I], load=I, inc=I, reset=O => o=[I, I]);  // load has precedence over inc
            }
        }
    }
}
