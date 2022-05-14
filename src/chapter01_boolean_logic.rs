use crate::hardware::{SystemBuilder, Wire};
use Bit::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Bit {
    O,
    I,
}

impl Default for Bit {
    fn default() -> Self {
        Bit::O
    }
}

pub trait Logic {
    fn nand(a: &Self, b: &Self) -> Self;

    fn mux(a: &Self, b: &Self, sel: Bit) -> Self;
}

impl Logic for Bit {
    fn nand(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (I, I) => O,
            _ => I,
        }
    }

    fn mux(a: &Self, b: &Self, sel: Bit) -> Self {
        nand(&nand(&sel, b), &nand(&not(&sel), a))
    }
}

impl<T: Logic> Logic for Vec<T> {
    fn nand(a: &Self, b: &Self) -> Self {
        a.iter().zip(b).map(|(ai, bi)| T::nand(ai, bi)).collect()
    }

    fn mux(a: &Self, b: &Self, sel: Bit) -> Self {
        a.iter()
            .zip(b)
            .map(|(ai, bi)| T::mux(ai, bi, sel))
            .collect()
    }
}

pub fn make_identity(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<Box<str>>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
) {
    sb.add_device(name, &[&a], &[&b], move |inp, out| {
        out.push(match inp {
            [I] => I,
            [O] => O,
            _ => unreachable!(),
        });
    })
}

pub fn make_nand(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<Box<str>>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
    o: &Wire<Bit>,
) {
    sb.add_device(name, &[&a, &b], &[&o], move |inp, out| {
        out.push(match inp {
            [I, I] => O,
            [O, O] | [O, I] | [I, O] => I,
            _ => unreachable!(),
        });
    })
}

pub fn make_not(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &Wire<Bit>,
    o: &Wire<Bit>,
) {
    make_nand(sb, name.into() + ".nand", a, a, o);
}

pub fn make_and(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
    o: &Wire<Bit>,
) {
    let name = name.into();
    let_wires!(no);
    make_nand(sb, name.clone() + ".nand", a, b, &no);
    make_not(sb, name + ".not", &no, o);
}

pub fn make_or(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
    o: &Wire<Bit>,
) {
    let name = name.into();
    let_wires!(na, nb);
    make_not(sb, name.clone() + ".notA", a, &na);
    make_not(sb, name.clone() + ".notB", b, &nb);
    make_nand(sb, name.clone() + ".nand", &na, &nb, o);
}

pub fn make_xor(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
    o: &Wire<Bit>,
) {
    let name = name.into();
    let_wires!(na, nb, anb, bna);
    make_not(sb, name.clone() + ".notA", a, &na);
    make_not(sb, name.clone() + ".notB", b, &nb);
    make_nand(sb, name.clone() + "nandAB", a, &nb, &anb);
    make_nand(sb, name.clone() + "nandBA", b, &na, &bna);
    make_nand(sb, name + "nandO", &anb, &bna, o);
}

pub fn make_mux(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
    sel: &Wire<Bit>,
    o: &Wire<Bit>,
) {
    let name = name.into();
    let_wires!(nsel, selb, sela);
    make_not(sb, name.clone() + ".not", sel, &nsel);
    make_nand(sb, name.clone() + "nandB", b, sel, &selb);
    make_nand(sb, name.clone() + "nandA", a, &nsel, &sela);
    make_nand(sb, name + "nandO", &sela, &selb, o);
}

pub fn make_demux(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    i: &Wire<Bit>,
    sel: &Wire<Bit>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
) {
    let name = name.into();
    let_wires!(nsel);
    make_not(sb, name.clone() + ".not", sel, &nsel);
    make_and(sb, name.clone() + ".andA", i, &nsel, a);
    make_and(sb, name + ".andB", i, &sel, b);
}

pub fn make_not_bus(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &[Wire<Bit>],
    b: &[Wire<Bit>],
) {
    let name = name.into();
    assert_eq!(a.len(), b.len());
    for (k, (ai, bi)) in a.iter().zip(b).enumerate() {
        make_not(sb, format!("{}.not[{}]", name, k), ai, bi);
    }
}

pub fn make_and_bus(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &[Wire<Bit>],
    b: &[Wire<Bit>],
    o: &[Wire<Bit>],
) {
    let name = name.into();
    assert_eq!(a.len(), b.len());
    assert_eq!(a.len(), o.len());
    for (k, ((ai, bi), oi)) in a.iter().zip(b).zip(o).enumerate() {
        make_and(sb, format!("{}.and[{}]", name, k), ai, bi, oi);
    }
}

pub fn make_or_bus(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &[Wire<Bit>],
    b: &[Wire<Bit>],
    o: &[Wire<Bit>],
) {
    let name = name.into();
    assert_eq!(a.len(), b.len());
    assert_eq!(a.len(), o.len());
    for (k, ((ai, bi), oi)) in a.iter().zip(b).zip(o).enumerate() {
        make_or(sb, format!("{}.or[{}]", name, k), ai, bi, oi);
    }
}

pub fn make_mux_bus(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &[Wire<Bit>],
    b: &[Wire<Bit>],
    sel: &Wire<Bit>,
    o: &[Wire<Bit>],
) {
    let name = name.into();
    assert_eq!(a.len(), b.len());
    assert_eq!(a.len(), o.len());
    for (k, ((ai, bi), oi)) in a.iter().zip(b).zip(o).enumerate() {
        make_mux(sb, format!("{}.mux[{}]", name, k), ai, bi, sel, oi);
    }
}

pub fn make_or_reduce(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &[Wire<Bit>],
    o: &Wire<Bit>,
) {
    assert!(a.len() >= 2);
    let name = name.into();

    let mut inputs: Vec<_> = a.iter().cloned().collect();

    let mut output = o.clone();
    let mut k = 0;

    while inputs.len() > 2 {
        let x = inputs.pop().unwrap();
        let y = Wire::new(O);
        make_or(sb, format!("{}.or[{}]", name, k), &x, &y, &output);
        output = y;
        k += 1;
    }

    let x = inputs.pop().unwrap();
    let y = inputs.pop().unwrap();
    make_or(sb, format!("{}.or[{}]", name, k), &x, &y, &output);
}

pub fn make_mux_multi(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    inputs: &[&[Wire<Bit>]],
    sel: &[Wire<Bit>],
    o: &[Wire<Bit>],
) {
    let name = name.into();
    assert_eq!(inputs.len(), 2usize.pow(sel.len() as u32));
    let width = o.len();
    for i in inputs {
        assert_eq!(i.len(), width);
    }

    let mut inputs: Vec<_> = inputs.iter().map(|i| i.to_vec()).collect();
    let mut sel = sel.to_vec();

    let mut k = 0;
    while sel.len() > 1 {
        let subsel = sel.pop().unwrap();
        let outputs: Vec<_> = inputs
            .chunks(2)
            .map(|chunk| match chunk {
                [a, b] => (a, b, {
                    let_buses!(out[width]);
                    out
                }),
                _ => unreachable!(),
            })
            .map(|(a, b, out)| {
                make_mux_bus(sb, format!("{}.bus-mux{}", name, k), a, b, &subsel, &out);
                k += 1;
                out
            })
            .collect();
        inputs = outputs;
    }
    assert_eq!(inputs.len(), 2);

    make_mux_bus(
        sb,
        format!("{}.bus-mux{}", name, k),
        &inputs[0],
        &inputs[1],
        &sel[0],
        o,
    );
}

pub fn make_demux_multi(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    input: &Wire<Bit>,
    sel: &[Wire<Bit>],
    out: &[Wire<Bit>],
) {
    let name = name.into();
    assert_eq!(out.len(), 2usize.pow(sel.len() as u32));

    let mut outputs: Vec<_> = out.to_vec();
    let mut sel = sel.to_vec();

    let mut k = 0;
    while sel.len() > 1 {
        let subsel = sel.pop().unwrap();
        let inputs: Vec<_> = outputs
            .chunks(2)
            .map(|chunk| match chunk {
                [a, b] => (
                    {
                        let_wires!(inp);
                        inp
                    },
                    a,
                    b,
                ),
                _ => unreachable!(),
            })
            .map(|(inp, a, b)| {
                make_demux(sb, format!("{}.demux{}", name, k), &inp, &subsel, a, b);
                k += 1;
                inp
            })
            .collect();
        outputs = inputs;
    }
    assert_eq!(outputs.len(), 2);

    make_demux(
        sb,
        format!("{}.demux{}", name, k),
        input,
        &sel[0],
        &outputs[0],
        &outputs[1],
    );
}

pub fn nand<T: Logic>(a: &T, b: &T) -> T {
    T::nand(a, b)
}

pub fn not<T: Logic>(a: &T) -> T {
    nand(a, a)
}

pub fn and<T: Logic>(a: &T, b: &T) -> T {
    not(&nand(a, b))
}

pub fn or<T: Logic>(a: &T, b: &T) -> T {
    nand(&not(a), &not(b))
}

pub fn xor<T: Logic>(a: &T, b: &T) -> T {
    nand(&nand(a, &not(b)), &nand(&not(a), b))
}

pub fn mux<T: Logic>(a: &T, b: &T, sel: Bit) -> T {
    T::mux(a, b, sel)
}

pub fn demux<T: Logic>(i: &T, sel: &T) -> (T, T) {
    let a = and(i, &not(sel));
    let b = and(i, sel);
    (a, b)
}

pub fn multi_or(input: &[Bit]) -> Bit {
    input.iter().fold(O, |x, y| or(&x, y))
}

pub fn multi_mux<T: Logic + Clone>(inputs: &[T], sel: &[Bit]) -> T {
    inputs[index(sel)].clone()
}

pub fn multi_demux(input: Bit, sel: &[Bit]) -> Vec<Bit> {
    let mut output = vec![O; 2usize.pow(sel.len() as u32)];
    output[index(sel)] = input;
    output
}

fn index(sel: &[Bit]) -> usize {
    let mut idx = 0;
    for s in sel.iter() {
        idx *= 2;
        match s {
            I => idx += 1,
            O => {}
        }
    }
    idx
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hardware::BusApi;

    #[test]
    fn nand_gate() {
        system! {
            sys
            wires { a, b, o }
            gates {
                make_nand("NAND", a, b, o);
            }
            body {
                assert_sim!(sys, a=O, b=O => o=I);
                assert_sim!(sys, a=O, b=I => o=I);
                assert_sim!(sys, a=I, b=O => o=I);
                assert_sim!(sys, a=I, b=I => o=O);
            }
        }
    }

    #[test]
    fn not_gate() {
        system! {
            sys
            wires { a, b }
            gates {
                make_not("NOT", a, b);
            }
            body {
                assert_sim!(sys, a=O => b=I);
                assert_sim!(sys, a=I => b=O);
            }
        }
    }

    #[test]
    fn and_gate() {
        system! {
            sys
            wires { a, b, o }
            gates {
                make_and("AND", a, b, o);
            }
            body {
                assert_sim!(sys, a=O, b=O => o=O);
                assert_sim!(sys, a=O, b=I => o=O);
                assert_sim!(sys, a=I, b=O => o=O);
                assert_sim!(sys, a=I, b=I => o=I);
            }
        }
    }

    #[test]
    fn or_gate() {
        system! {
            sys
            wires { a, b, o }
            gates {
                make_or("OR", a, b, o);
            }
            body {
                assert_sim!(sys, a=O, b=O => o=O);
                assert_sim!(sys, a=O, b=I => o=I);
                assert_sim!(sys, a=I, b=O => o=I);
                assert_sim!(sys, a=I, b=I => o=I);
            }
        }
    }

    #[test]
    fn xor_gate() {
        system! {
            sys
            wires { a, b, o }
            gates {
                make_xor("XOR", a, b, o);
            }
            body {
                assert_sim!(sys, a=O, b=O => o=O);
                assert_sim!(sys, a=O, b=I => o=I);
                assert_sim!(sys, a=I, b=O => o=I);
                assert_sim!(sys, a=I, b=I => o=O);
            }
        }
    }

    #[test]
    fn mux_gate() {
        system! {
            sys
            wires { a, b, sel, o }
            gates {
                make_mux("MUX", a, b, sel, o);
            }
            body {
                assert_sim!(sys, a=O, b=O , sel=O => o=O);
                assert_sim!(sys, a=O, b=I , sel=O => o=O);
                assert_sim!(sys, a=I, b=O , sel=O => o=I);
                assert_sim!(sys, a=I, b=I , sel=O => o=I);
                assert_sim!(sys, a=O, b=O , sel=I => o=O);
                assert_sim!(sys, a=O, b=I , sel=I => o=I);
                assert_sim!(sys, a=I, b=O , sel=I => o=O);
                assert_sim!(sys, a=I, b=I , sel=I => o=I);
            }
        }
    }

    #[test]
    fn demux_gate() {
        system! {
            sys
            wires { i, sel, a, b }
            gates {
                make_demux("DEMUX", i, sel, a, b);
            }
            body {
                assert_sim!(sys, i=O, sel=O => a=O, b=O);
                assert_sim!(sys, i=O, sel=I => a=O, b=O);
                assert_sim!(sys, i=I, sel=O => a=I, b=O);
                assert_sim!(sys, i=I, sel=I => a=O, b=I);
            }
        }
    }

    #[test]
    fn multi_bit_not() {
        system! {
            sys
            wires { }
            buses { a[2], b[2] }
            gates {
                make_not_bus("NOT", a, b);
            }
            body {
                assert_sim!(sys, a=&[O, O] => b=[I, I]);
                assert_sim!(sys, a=&[O, I] => b=[I, O]);
                assert_sim!(sys, a=&[I, O] => b=[O, I]);
                assert_sim!(sys, a=&[I, I] => b=[O, O]);
            }
        }
    }

    #[test]
    fn multi_bit_and() {
        system! {
            sys
            buses { a[4], b[4], o[4] }
            gates {
                make_and_bus("AND", a, b, o);
            }
            body {
                assert_sim!(sys, a=&[O, O, I, I], b=&[O, I, O, I] => o=[O, O, O, I]);
            }
        }
    }

    #[test]
    fn multi_bit_or() {
        system! {
            sys
            buses { a[4], b[4], o[4] }
            gates {
                make_or_bus("NOT", a, b, o);
            }
            body {
                assert_sim!(sys, a=&[O, O, I, I], b=&[O, I, O, I] => o=[O, I, I, I]);
            }
        }
    }

    #[test]
    fn multi_bit_mux() {
        system! {
            sys
            wires { sel }
            buses { a[4], b[4], o[4] }
            gates {
                make_mux_bus("NOT", a, b, sel, o);
            }
            body {
                assert_sim!(sys, a=&[O, O, I, I], b=&[O, I, O, I], sel=O => o=[O, O, I, I]);
                assert_sim!(sys, a=&[O, O, I, I], b=&[O, I, O, I], sel=I => o=[O, I, O, I]);
            }
        }
    }

    #[test]
    fn multi_way_or() {
        system! {
            sys
            wires { o }
            buses { a[3] }
            gates {
                make_or_reduce("NOT", a, o);
            }
            body {
                assert_sim!(sys, a=&[O, O, O] => o=O);
                assert_sim!(sys, a=&[I, O, O] => o=I);
                assert_sim!(sys, a=&[O, I, O] => o=I);
                assert_sim!(sys, a=&[O, O, I] => o=I);
                assert_sim!(sys, a=&[O, I, I] => o=I);
                assert_sim!(sys, a=&[I, O, I] => o=I);
                assert_sim!(sys, a=&[I, I, O] => o=I);
                assert_sim!(sys, a=&[I, I, I] => o=I);
            }
        }
    }

    #[test]
    fn mux_4way_nbit() {
        system! {
            sys
            wires {}
            buses { a[4], b[4], c[4], d[4], sel[2], o[4]}
            setup {
                let inputs = [a.as_slice(), b.as_slice(), c.as_slice(), d.as_slice()];
            }
            gates {
                make_mux_multi("MULTIMUX", inputs, sel, o);
            }
            body {
                assert_sim!(sys,
                    a=&[I, O, O, O], b=&[O, I, O, O], c=&[O, O, I, O], d=&[O, O, O, I],
                    sel=&[O, O] => o=[I, O, O, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O], b=&[O, I, O, O], c=&[O, O, I, O], d=&[O, O, O, I],
                    sel=&[O, I] => o=[O, I, O, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O], b=&[O, I, O, O], c=&[O, O, I, O], d=&[O, O, O, I],
                    sel=&[I, O] => o=[O, O, I, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O], b=&[O, I, O, O], c=&[O, O, I, O], d=&[O, O, O, I],
                    sel=&[I, I] => o=[O, O, O, I]);
            }
        }
    }

    #[test]
    fn mux_8way_nbit() {
        system! {
            sys
            wires {}
            buses { a[8], b[8], c[8], d[8], e[8], f[8], g[8], h[8], sel[3], o[8]}
            setup {
                let inputs = [a.as_slice(), b.as_slice(), c.as_slice(), d.as_slice(),
                              e.as_slice(), f.as_slice(), g.as_slice(), h.as_slice()];
            }
            gates {
                make_mux_multi("MULTIMUX", inputs, sel, o);
            }
            body {
                assert_sim!(sys,
                    a=&[I, O, O, O, O, O, O, O], b=&[O, I, O, O, O, O, O, O], c=&[O, O, I, O, O, O, O, O], d=&[O, O, O, I, O, O, O, O],
                    e=&[O, O, O, O, I, O, O, O], f=&[O, O, O, O, O, I, O, O], g=&[O, O, O, O, O, O, I, O], h=&[O, O, O, O, O, O, O, I],
                    sel=&[O, O, O] => o=[I, O, O, O, O, O, O, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O, O, O, O, O], b=&[O, I, O, O, O, O, O, O], c=&[O, O, I, O, O, O, O, O], d=&[O, O, O, I, O, O, O, O],
                    e=&[O, O, O, O, I, O, O, O], f=&[O, O, O, O, O, I, O, O], g=&[O, O, O, O, O, O, I, O], h=&[O, O, O, O, O, O, O, I],
                    sel=&[O, O, I] => o=[O, I, O, O, O, O, O, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O, O, O, O, O], b=&[O, I, O, O, O, O, O, O], c=&[O, O, I, O, O, O, O, O], d=&[O, O, O, I, O, O, O, O],
                    e=&[O, O, O, O, I, O, O, O], f=&[O, O, O, O, O, I, O, O], g=&[O, O, O, O, O, O, I, O], h=&[O, O, O, O, O, O, O, I],
                    sel=&[O, I, O] => o=[O, O, I, O, O, O, O, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O, O, O, O, O], b=&[O, I, O, O, O, O, O, O], c=&[O, O, I, O, O, O, O, O], d=&[O, O, O, I, O, O, O, O],
                    e=&[O, O, O, O, I, O, O, O], f=&[O, O, O, O, O, I, O, O], g=&[O, O, O, O, O, O, I, O], h=&[O, O, O, O, O, O, O, I],
                    sel=&[O, I, I] => o=[O, O, O, I, O, O, O, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O, O, O, O, O], b=&[O, I, O, O, O, O, O, O], c=&[O, O, I, O, O, O, O, O], d=&[O, O, O, I, O, O, O, O],
                    e=&[O, O, O, O, I, O, O, O], f=&[O, O, O, O, O, I, O, O], g=&[O, O, O, O, O, O, I, O], h=&[O, O, O, O, O, O, O, I],
                    sel=&[I, O, O] => o=[O, O, O, O, I, O, O, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O, O, O, O, O], b=&[O, I, O, O, O, O, O, O], c=&[O, O, I, O, O, O, O, O], d=&[O, O, O, I, O, O, O, O],
                    e=&[O, O, O, O, I, O, O, O], f=&[O, O, O, O, O, I, O, O], g=&[O, O, O, O, O, O, I, O], h=&[O, O, O, O, O, O, O, I],
                    sel=&[I, O, I] => o=[O, O, O, O, O, I, O, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O, O, O, O, O], b=&[O, I, O, O, O, O, O, O], c=&[O, O, I, O, O, O, O, O], d=&[O, O, O, I, O, O, O, O],
                    e=&[O, O, O, O, I, O, O, O], f=&[O, O, O, O, O, I, O, O], g=&[O, O, O, O, O, O, I, O], h=&[O, O, O, O, O, O, O, I],
                    sel=&[I, I, O] => o=[O, O, O, O, O, O, I, O]);
                assert_sim!(sys,
                    a=&[I, O, O, O, O, O, O, O], b=&[O, I, O, O, O, O, O, O], c=&[O, O, I, O, O, O, O, O], d=&[O, O, O, I, O, O, O, O],
                    e=&[O, O, O, O, I, O, O, O], f=&[O, O, O, O, O, I, O, O], g=&[O, O, O, O, O, O, I, O], h=&[O, O, O, O, O, O, O, I],
                    sel=&[I, I, I] => o=[O, O, O, O, O, O, O, I]);
            }
        }
    }

    #[test]
    fn demux_4way_1bit() {
        system! {
            sys
            wires { input }
            buses { sel[2], out[4] }
            gates {
                make_demux_multi("DEMUX", input, sel, out);
            }
            body {
                assert_sim!(sys, input=O, sel=&[O, O] => out=[O, O, O, O]);
                assert_sim!(sys, input=O, sel=&[I, I] => out=[O, O, O, O]);
                assert_sim!(sys, input=I, sel=&[O, O] => out=[I, O, O, O]);
                assert_sim!(sys, input=I, sel=&[O, I] => out=[O, I, O, O]);
                assert_sim!(sys, input=I, sel=&[I, O] => out=[O, O, I, O]);
                assert_sim!(sys, input=I, sel=&[I, I] => out=[O, O, O, I]);
            }
        }
    }

    #[test]
    fn demux_8way_1bit() {
        system! {
            sys
            wires { input }
            buses { sel[3], out[8] }
            gates {
                make_demux_multi("DEMUX", input, sel, out);
            }
            body {
                assert_sim!(sys, input=O, sel=&[O, O, O] => out=[O, O, O, O, O, O, O, O]);
                assert_sim!(sys, input=O, sel=&[I, I, O] => out=[O, O, O, O, O, O, O, O]);
                assert_sim!(sys, input=I, sel=&[O, O, O] => out=[I, O, O, O, O, O, O, O]);
                assert_sim!(sys, input=I, sel=&[O, O, I] => out=[O, I, O, O, O, O, O, O]);
                assert_sim!(sys, input=I, sel=&[O, I, O] => out=[O, O, I, O, O, O, O, O]);
                assert_sim!(sys, input=I, sel=&[O, I, I] => out=[O, O, O, I, O, O, O, O]);
                assert_sim!(sys, input=I, sel=&[I, O, O] => out=[O, O, O, O, I, O, O, O]);
                assert_sim!(sys, input=I, sel=&[I, O, I] => out=[O, O, O, O, O, I, O, O]);
                assert_sim!(sys, input=I, sel=&[I, I, O] => out=[O, O, O, O, O, O, I, O]);
                assert_sim!(sys, input=I, sel=&[I, I, I] => out=[O, O, O, O, O, O, O, I]);
            }
        }
    }
}
