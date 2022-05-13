use crate::chapter01_boolean_logic::Bit::O;
use crate::chapter01_boolean_logic::{and, multi_or, mux, not, or, xor, Bit, make_xor, make_and, make_or};
use crate::hardware::{SystemBuilder, Wire};

pub fn make_half_adder(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
    s: &Wire<Bit>,
    c: &Wire<Bit>,
) {
    let name = name.into();
    make_xor(sb, format!("{}.add", name), a, b, s);
    make_and(sb, format!("{}.carry", name), a, b, c)
}

pub fn make_full_adder(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &Wire<Bit>,
    b: &Wire<Bit>,
    c: &Wire<Bit>,
    sum: &Wire<Bit>,
    carry: &Wire<Bit>,
) {
    let name = name.into();
    let_wires!(s1, c1, c2);
    make_half_adder(sb, format!("{}.add1", name), a, b, &s1, &c1);
    make_half_adder(sb, format!("{}.add2", name), &s1, c, sum, &c2);
    make_or(sb, format!("{}.either_carry", name), &c1, &c2, carry);
}

pub fn make_adder(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &[Wire<Bit>],
    b: &[Wire<Bit>],
    c: &[Wire<Bit>],
) {
    let name = name.into();
    let_wires!(carry);
    let mut carry = carry;
    make_half_adder(sb, format!("{}.add0", name), &a[0], &b[0], &c[0], &carry);
    for (i, ((ai, bi), ci)) in a.iter().zip(b).zip(c).enumerate().skip(1) {
        let_wires!(new_carry);
        make_full_adder(sb, format!("{}.add{}", name, i), ai, bi, &carry, ci, &new_carry);
        carry = new_carry;
    }
}

#[derive(Debug, PartialEq)]
pub struct Adder {
    pub sum: Bit,
    pub carry: Bit,
}

impl Adder {
    pub fn half(a: Bit, b: Bit) -> Self {
        Adder {
            sum: xor(&a, &b),
            carry: and(&a, &b),
        }
    }

    pub fn full(a: Bit, b: Bit, c: Bit) -> Self {
        let first = Adder::half(a, b);
        let second = Adder::half(c, first.sum);
        Adder {
            sum: second.sum,
            carry: or(&first.carry, &second.carry),
        }
    }
}

fn multi_add(a: &[Bit], b: &[Bit]) -> Vec<Bit> {
    let mut result = vec![];
    let mut carry = O;

    for (&ai, &bi) in a.iter().zip(b) {
        let x = Adder::full(ai, bi, carry);
        result.push(x.sum);
        carry = x.carry;
    }

    result
}

#[derive(Debug, PartialEq)]
pub struct Alu {
    out: Vec<Bit>,
    zr: Bit,
    ng: Bit,
}

impl Alu {
    pub fn calc(
        x: &Vec<Bit>,
        y: &Vec<Bit>,
        zx: Bit,
        nx: Bit,
        zy: Bit,
        ny: Bit,
        f: Bit,
        no: Bit,
    ) -> Self {
        let x = set_zero(zx, x);
        let x = negate(nx, &x);
        let y = set_zero(zy, y);
        let y = negate(ny, &y);

        let added = multi_add(&x, &y);
        let anded = and(&x, &y);

        let out = mux(&anded, &added, f);
        let out = negate(no, &out);

        Alu {
            zr: not(&multi_or(&out)),
            ng: msb(&out),
            out,
        }
    }
}

fn set_zero(z: Bit, input: &Vec<Bit>) -> Vec<Bit> {
    let n = input.len();
    and(input, &vec![not(&z); n])
}

fn negate(c: Bit, input: &Vec<Bit>) -> Vec<Bit> {
    let n = input.len();
    or(
        &and(&vec![c; n], &not(&input)),
        &and(&vec![not(&c); n], input),
    )
}

fn msb(input: &[Bit]) -> Bit {
    *input.last().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use Bit::{I, O};
    use crate::hardware::BusApi;

    #[test]
    fn half_adder() {
        system! {
            sys
            wires { a, b, s, c }
            gates {
                make_half_adder("HALFADD", a, b, s, c);
            }
            body {
                assert_sim!(sys, a=O, b=O => s=O, c=O);
                assert_sim!(sys, a=O, b=I => s=I, c=O);
                assert_sim!(sys, a=I, b=O => s=I, c=O);
                assert_sim!(sys, a=I, b=I => s=O, c=I);
            }
        }
    }

    #[test]
    fn full_adder() {
        system! {
            sys
            wires { a, b, c, s, carry }
            gates {
                make_full_adder("ADD", a, b, c, s, carry);
            }
            body {
                assert_sim!(sys, a=O, b=O, c=O => s=O, carry=O);
                assert_sim!(sys, a=O, b=O, c=I => s=I, carry=O);
                assert_sim!(sys, a=O, b=I, c=O => s=I, carry=O);
                assert_sim!(sys, a=O, b=I, c=I => s=O, carry=I);
                assert_sim!(sys, a=I, b=O, c=O => s=I, carry=O);
                assert_sim!(sys, a=I, b=O, c=I => s=O, carry=I);
                assert_sim!(sys, a=I, b=I, c=O => s=O, carry=I);
                assert_sim!(sys, a=I, b=I, c=I => s=I, carry=I);
            }
        }
    }

    #[test]
    fn test_multi_add() {
        system! {
            sys
            buses { a[4], b[4], c[4]}
            gates {
                make_adder("ADD", a, b, c);
            }
            body {
                assert_sim!(sys, a=&[O, O, O, O], b=&[O, O, O, O] => c=[O, O, O, O]);
                assert_sim!(sys, a=&[O, O, O, O], b=&[I, I, I, I] => c=[I, I, I, I]);
                assert_sim!(sys, a=&[O, I, I, O], b=&[O, O, O, O] => c=[O, I, I, O]);
                assert_sim!(sys, a=&[I, O, O, I], b=&[I, O, O, I] => c=[O, I, O, O]);
                assert_sim!(sys, a=&[I, I, I, O], b=&[I, O, O, O] => c=[O, O, O, I]);
                assert_sim!(sys, a=&[I, O, O, O], b=&[I, I, I, O] => c=[O, O, O, I]);
                assert_sim!(sys, a=&[I, I, I, O], b=&[I, I, I, O] => c=[O, I, I, I]);
                assert_sim!(sys, a=&[I, I, I, I], b=&[I, I, I, I] => c=[O, I, I, I]);
            }
        }
    }

    #[test]
    fn test_alu() {
        // 0
        assert_eq!(
            Alu::calc(&vec![I, I], &vec![I, I], I, O, I, O, I, O),
            Alu {
                out: vec![O, O],
                zr: I,
                ng: O
            }
        );

        // 1
        assert_eq!(
            Alu::calc(&vec![I, I], &vec![I, I], I, I, I, I, I, I),
            Alu {
                out: vec![I, O],
                zr: O,
                ng: O
            }
        );

        // -1
        assert_eq!(
            Alu::calc(&vec![I, I], &vec![I, I], I, I, I, O, I, O),
            Alu {
                out: vec![I, I],
                zr: O,
                ng: I
            }
        );

        // x
        assert_eq!(
            Alu::calc(&vec![O, I, O], &vec![I, O, I], O, O, I, I, O, O),
            Alu {
                out: vec![O, I, O],
                zr: O,
                ng: O
            }
        );

        // y
        assert_eq!(
            Alu::calc(&vec![I, O, I], &vec![O, I, O], I, I, O, O, O, O),
            Alu {
                out: vec![O, I, O],
                zr: O,
                ng: O
            }
        );

        // !x
        assert_eq!(
            Alu::calc(&vec![I, O, I], &vec![O, I, O], O, O, I, I, O, I),
            Alu {
                out: vec![O, I, O],
                zr: O,
                ng: O
            }
        );

        // !y
        assert_eq!(
            Alu::calc(&vec![O, I, O], &vec![I, O, I], I, I, O, O, O, I),
            Alu {
                out: vec![O, I, O],
                zr: O,
                ng: O
            }
        );

        // -x
        assert_eq!(
            Alu::calc(&vec![I, I, O], &vec![I, I, I], O, O, I, I, I, I),
            Alu {
                out: vec![I, O, I],
                zr: O,
                ng: I
            }
        );

        // -y
        assert_eq!(
            Alu::calc(&vec![O, O, O], &vec![I, I, I], I, I, O, O, I, I),
            Alu {
                out: vec![I, O, O],
                zr: O,
                ng: O
            }
        );

        // x+1
        assert_eq!(
            Alu::calc(&vec![I, O, O], &vec![I, I, I], O, I, I, I, I, I),
            Alu {
                out: vec![O, I, O],
                zr: O,
                ng: O
            }
        );

        // y+1
        assert_eq!(
            Alu::calc(&vec![I, O, O], &vec![I, I, I], I, I, O, I, I, I),
            Alu {
                out: vec![O, O, O],
                zr: I,
                ng: O
            }
        );

        // x-1
        assert_eq!(
            Alu::calc(&vec![I, O, O], &vec![O, O, O], O, O, I, I, I, O),
            Alu {
                out: vec![O, O, O],
                zr: I,
                ng: O
            }
        );

        // y-1
        assert_eq!(
            Alu::calc(&vec![I, O, O], &vec![O, O, O], I, I, O, O, I, O),
            Alu {
                out: vec![I, I, I],
                zr: O,
                ng: I
            }
        );

        // x+y
        assert_eq!(
            Alu::calc(&vec![I, O, O], &vec![O, I, O], O, O, O, O, I, O),
            Alu {
                out: vec![I, I, O],
                zr: O,
                ng: O
            }
        );

        // x-y
        assert_eq!(
            Alu::calc(&vec![I, O, O], &vec![O, I, O], O, I, O, O, I, I),
            Alu {
                out: vec![I, I, I],
                zr: O,
                ng: I
            }
        );

        // y-x
        assert_eq!(
            Alu::calc(&vec![I, O, O], &vec![O, I, O], O, O, O, I, I, I),
            Alu {
                out: vec![I, O, O],
                zr: O,
                ng: O
            }
        );

        // x&y
        assert_eq!(
            Alu::calc(&vec![O, O, I, I], &vec![O, I, O, I], O, O, O, O, O, O),
            Alu {
                out: vec![O, O, O, I],
                zr: O,
                ng: I
            }
        );

        // x|y
        assert_eq!(
            Alu::calc(&vec![O, O, I, I], &vec![O, I, O, I], O, I, O, I, O, I),
            Alu {
                out: vec![O, I, I, I],
                zr: O,
                ng: I
            }
        );
    }

    #[test]
    fn test_set_zero() {
        assert_eq!(set_zero(O, &vec![I, O, I, O]), vec![I, O, I, O]);
        assert_eq!(set_zero(I, &vec![I, O, I, O]), vec![O, O, O, O]);
    }

    #[test]
    fn test_negate() {
        assert_eq!(negate(O, &vec![I, O, I, O]), vec![I, O, I, O]);
        assert_eq!(negate(I, &vec![I, O, I, O]), vec![O, I, O, I]);
    }
}
