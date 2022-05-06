use crate::chapter01_boolean_logic::Bit::O;
use crate::chapter01_boolean_logic::{and, multi_or, mux, not, or, xor, Bit};

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

    #[test]
    fn half_adder() {
        assert_eq!(Adder::half(O, O), Adder { sum: O, carry: O });
        assert_eq!(Adder::half(O, I), Adder { sum: I, carry: O });
        assert_eq!(Adder::half(I, O), Adder { sum: I, carry: O });
        assert_eq!(Adder::half(I, I), Adder { sum: O, carry: I });
    }

    #[test]
    fn full_adder() {
        assert_eq!(Adder::full(O, O, O), Adder { sum: O, carry: O });
        assert_eq!(Adder::full(O, O, I), Adder { sum: I, carry: O });
        assert_eq!(Adder::full(O, I, O), Adder { sum: I, carry: O });
        assert_eq!(Adder::full(O, I, I), Adder { sum: O, carry: I });
        assert_eq!(Adder::full(I, O, O), Adder { sum: I, carry: O });
        assert_eq!(Adder::full(I, O, I), Adder { sum: O, carry: I });
        assert_eq!(Adder::full(I, I, O), Adder { sum: O, carry: I });
        assert_eq!(Adder::full(I, I, I), Adder { sum: I, carry: I });
    }

    #[test]
    fn test_multi_add() {
        assert_eq!(multi_add(&[], &[]), vec![]);
        assert_eq!(multi_add(&[O], &[O]), vec![O]);
        assert_eq!(multi_add(&[I], &[O]), vec![I]);
        assert_eq!(multi_add(&[O], &[I]), vec![I]);
        assert_eq!(multi_add(&[I], &[I]), vec![O]);
        assert_eq!(multi_add(&[O, O], &[O, O]), vec![O, O]);
        assert_eq!(multi_add(&[I, O], &[O, I]), vec![I, I]);
        assert_eq!(multi_add(&[I, O], &[I, O]), vec![O, I]);
        assert_eq!(multi_add(&[I, I], &[I, O]), vec![O, O]);
        assert_eq!(multi_add(&[I, I], &[I, I]), vec![O, I]);
        assert_eq!(multi_add(&[I, I, I, O], &[I, O, O, O]), vec![O, O, O, I]);
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
