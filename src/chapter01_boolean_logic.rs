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
    use crate::hardware::{SystemBuilder, Wire};

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
        assert_eq!(not(&vec![O; 16]), vec![I; 16]);
        assert_eq!(not(&vec![I; 16]), vec![O; 16]);
    }

    #[test]
    fn multi_bit_and() {
        assert_eq!(and(&vec![O, O, I, I], &vec![O, I, O, I]), vec![O, O, O, I]);
    }

    #[test]
    fn multi_bit_or() {
        assert_eq!(or(&vec![O, O, I, I], &vec![O, I, O, I]), vec![O, I, I, I]);
    }

    #[test]
    fn multi_bit_mux() {
        assert_eq!(
            mux(&vec![O, O, I, I], &vec![O, I, O, I], O),
            vec![O, O, I, I]
        );
        assert_eq!(
            mux(&vec![O, O, I, I], &vec![O, I, O, I], I),
            vec![O, I, O, I]
        );
    }

    #[test]
    fn multi_way_or() {
        assert_eq!(multi_or(&[O, O, O]), O);
    }

    #[test]
    fn mux_4way_nbit() {
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O],
                    vec![O, I, O, O],
                    vec![O, O, I, O],
                    vec![O, O, O, I]
                ],
                &[O, O]
            ),
            vec![I, O, O, O]
        );
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O],
                    vec![O, I, O, O],
                    vec![O, O, I, O],
                    vec![O, O, O, I]
                ],
                &[O, I]
            ),
            vec![O, I, O, O]
        );
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O],
                    vec![O, I, O, O],
                    vec![O, O, I, O],
                    vec![O, O, O, I]
                ],
                &[I, O]
            ),
            vec![O, O, I, O]
        );
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O],
                    vec![O, I, O, O],
                    vec![O, O, I, O],
                    vec![O, O, O, I]
                ],
                &[I, I]
            ),
            vec![O, O, O, I]
        );
    }

    #[test]
    fn mux_8way_nbit() {
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O, O, O, O, O],
                    vec![O, I, O, O, O, O, O, O],
                    vec![O, O, I, O, O, O, O, O],
                    vec![O, O, O, I, O, O, O, O],
                    vec![O, O, O, O, I, O, O, O],
                    vec![O, O, O, O, O, I, O, O],
                    vec![O, O, O, O, O, O, I, O],
                    vec![O, O, O, O, O, O, O, I]
                ],
                &[O, O, O]
            ),
            vec![I, O, O, O, O, O, O, O],
        );
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O, O, O, O, O],
                    vec![O, I, O, O, O, O, O, O],
                    vec![O, O, I, O, O, O, O, O],
                    vec![O, O, O, I, O, O, O, O],
                    vec![O, O, O, O, I, O, O, O],
                    vec![O, O, O, O, O, I, O, O],
                    vec![O, O, O, O, O, O, I, O],
                    vec![O, O, O, O, O, O, O, I]
                ],
                &[O, O, I]
            ),
            vec![O, I, O, O, O, O, O, O],
        );
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O, O, O, O, O],
                    vec![O, I, O, O, O, O, O, O],
                    vec![O, O, I, O, O, O, O, O],
                    vec![O, O, O, I, O, O, O, O],
                    vec![O, O, O, O, I, O, O, O],
                    vec![O, O, O, O, O, I, O, O],
                    vec![O, O, O, O, O, O, I, O],
                    vec![O, O, O, O, O, O, O, I]
                ],
                &[O, I, O]
            ),
            vec![O, O, I, O, O, O, O, O],
        );
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O, O, O, O, O],
                    vec![O, I, O, O, O, O, O, O],
                    vec![O, O, I, O, O, O, O, O],
                    vec![O, O, O, I, O, O, O, O],
                    vec![O, O, O, O, I, O, O, O],
                    vec![O, O, O, O, O, I, O, O],
                    vec![O, O, O, O, O, O, I, O],
                    vec![O, O, O, O, O, O, O, I]
                ],
                &[I, O, O]
            ),
            vec![O, O, O, O, I, O, O, O],
        );
        assert_eq!(
            multi_mux(
                &[
                    vec![I, O, O, O, O, O, O, O],
                    vec![O, I, O, O, O, O, O, O],
                    vec![O, O, I, O, O, O, O, O],
                    vec![O, O, O, I, O, O, O, O],
                    vec![O, O, O, O, I, O, O, O],
                    vec![O, O, O, O, O, I, O, O],
                    vec![O, O, O, O, O, O, I, O],
                    vec![O, O, O, O, O, O, O, I]
                ],
                &[I, I, I]
            ),
            vec![O, O, O, O, O, O, O, I],
        );
    }

    #[test]
    fn demux_4way_1bit() {
        assert_eq!(multi_demux(O, &[O, O]), vec![O, O, O, O]);
        assert_eq!(multi_demux(O, &[I, I]), vec![O, O, O, O]);
        assert_eq!(multi_demux(I, &[O, O]), vec![I, O, O, O]);
        assert_eq!(multi_demux(I, &[O, I]), vec![O, I, O, O]);
        assert_eq!(multi_demux(I, &[I, O]), vec![O, O, I, O]);
        assert_eq!(multi_demux(I, &[I, I]), vec![O, O, O, I]);
    }

    #[test]
    fn demux_8way_1bit() {
        assert_eq!(multi_demux(O, &[O, O, O]), vec![O, O, O, O, O, O, O, O]);
        assert_eq!(multi_demux(O, &[I, I, I]), vec![O, O, O, O, O, O, O, O]);
        assert_eq!(multi_demux(I, &[O, O, O]), vec![I, O, O, O, O, O, O, O]);
        assert_eq!(multi_demux(I, &[O, O, I]), vec![O, I, O, O, O, O, O, O]);
        assert_eq!(multi_demux(I, &[O, I, O]), vec![O, O, I, O, O, O, O, O]);
        assert_eq!(multi_demux(I, &[O, I, I]), vec![O, O, O, I, O, O, O, O]);
        assert_eq!(multi_demux(I, &[I, O, O]), vec![O, O, O, O, I, O, O, O]);
        assert_eq!(multi_demux(I, &[I, O, I]), vec![O, O, O, O, O, I, O, O]);
        assert_eq!(multi_demux(I, &[I, I, O]), vec![O, O, O, O, O, O, I, O]);
        assert_eq!(multi_demux(I, &[I, I, I]), vec![O, O, O, O, O, O, O, I]);
    }
}
