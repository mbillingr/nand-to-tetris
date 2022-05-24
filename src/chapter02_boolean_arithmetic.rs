use crate::chapter01_boolean_logic::Bit::{I, O};
use crate::chapter01_boolean_logic::{
    make_and, make_and_bus, make_identity, make_mux_bus, make_not, make_or, make_or_reduce,
    make_xor, Bit,
};
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
    assert_eq!(a.len(), b.len());
    assert_eq!(a.len(), c.len());
    let name = name.into();
    let_wires!(carry);
    let mut carry = carry;
    make_half_adder(sb, format!("{}.add0", name), &a[0], &b[0], &c[0], &carry);
    for (i, ((ai, bi), ci)) in a.iter().zip(b).zip(c).enumerate().skip(1) {
        let_wires!(new_carry);
        make_full_adder(
            sb,
            format!("{}.add{}", name, i),
            ai,
            bi,
            &carry,
            ci,
            &new_carry,
        );
        carry = new_carry;
    }
}

pub fn make_incrementer(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    a: &[Wire<Bit>],
    b: &[Wire<Bit>],
) {
    assert_eq!(a.len(), b.len());
    let name = name.into();

    make_not(sb, format!("{}.add0", name), &a[0], &b[0]);
    let mut carry = a[0].clone();

    for (i, (ai, bi)) in a.iter().zip(b).enumerate().skip(1) {
        let_wires!(new_carry);
        make_half_adder(sb, format!("{}.add{}", name, i), ai, &carry, bi, &new_carry);
        carry = new_carry;
    }
}

pub fn make_alu(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    x: &[Wire<Bit>],
    y: &[Wire<Bit>],
    zx: &Wire<Bit>,
    nx: &Wire<Bit>,
    zy: &Wire<Bit>,
    ny: &Wire<Bit>,
    f: &Wire<Bit>,
    no: &Wire<Bit>,
    out: &[Wire<Bit>],
    zr: &Wire<Bit>,
    ng: &Wire<Bit>,
) {
    let width = out.len();
    let name = name.into();

    let_buses!(x_z[width], y_z[width]);
    set_zero(sb, format!("{}.zerox", name), x, zx, &x_z);
    set_zero(sb, format!("{}.zeroy", name), y, zy, &y_z);

    let_buses!(x_n[width], y_n[width]);
    negate(sb, format!("{}.negx", name), &x_z, nx, &x_n);
    negate(sb, format!("{}.negy", name), &y_z, ny, &y_n);

    let_buses!(fand[width], fadd[width]);
    make_and_bus(sb, format!("{}.fand", name), &x_n, &y_n, &fand);
    make_adder(sb, format!("{}.fadd", name), &x_n, &y_n, &fadd);

    let_buses!(pre_out[width]);
    make_mux_bus(sb, format!("{}.fmux", name), &fand, &fadd, f, &pre_out);

    negate(sb, format!("{}.nego", name), &pre_out, no, &out);

    let_wires!(out_nz);
    make_or_reduce(sb, format!("{}.notzero", name), out, &out_nz);
    make_not(sb, format!("{}.iszero", name), &out_nz, zr);

    make_identity(sb, format!("{}.isneg", name), &out[width - 1], ng);
}

pub fn set_zero(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    input: &[Wire<Bit>],
    sel: &Wire<Bit>,
    out: &[Wire<Bit>],
) {
    assert_eq!(input.len(), out.len());
    let name = name.into();

    let_wires!(nsel);
    make_not(sb, format!("{}.isel", name), sel, &nsel);

    for (k, (i, o)) in input.iter().zip(out).enumerate() {
        make_and(sb, format!("{}[{}]", name, k), i, &nsel, o);
    }
}

pub fn negate(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    input: &[Wire<Bit>],
    sel: &Wire<Bit>,
    out: &[Wire<Bit>],
) {
    assert_eq!(input.len(), out.len());
    let name = name.into();
    for (k, (i, o)) in input.iter().zip(out).enumerate() {
        make_xor(sb, format!("{}[{}]", name, k), i, sel, o);
    }
}

pub fn bus_as_number(bits: &[Bit]) -> u16 {
    let mut num = 0;
    for &b in bits.iter().rev() {
        num *= 2;
        if b == I {
            num += 1;
        }
    }
    num
}

pub fn number_to_bus(mut num: u16, width: usize, bits: &mut Vec<Bit>) {
    for _ in 0..width {
        if num % 2 == 1 {
            bits.push(I);
        } else {
            bits.push(O);
        }
        num /= 2;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hardware::BusApi;
    use Bit::{I, O};

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
    fn test_incrementer() {
        system! {
            sys
            buses { a[3], b[3]}
            gates {
                make_incrementer("INC", a, b);
            }
            body {
                assert_sim!(sys, a=&[O, O, O] => b=[I, O, O]);
                assert_sim!(sys, a=&[I, O, O] => b=[O, I, O]);
                assert_sim!(sys, a=&[O, I, O] => b=[I, I, O]);
                assert_sim!(sys, a=&[I, I, O] => b=[O, O, I]);
                assert_sim!(sys, a=&[O, O, I] => b=[I, O, I]);
                assert_sim!(sys, a=&[I, O, I] => b=[O, I, I]);
                assert_sim!(sys, a=&[O, I, I] => b=[I, I, I]);
                assert_sim!(sys, a=&[I, I, I] => b=[O, O, O]);
            }
        }
    }

    #[test]
    fn test_alu() {
        system! {
            sys
            wires { zx, nx, zy, ny, f, no, zr, ng }
            buses { x[2], y[2], out[2]}
            gates {
                make_alu("ALU", x, y, zx, nx, zy, ny, f, no, out, zr, ng);
            }
            body {
                // 0
                assert_sim!(sys,
                    x=&[I, I], y=&[I, I], zx=I, nx=O, zy=I, ny=O, f=I, no=O
                    => out=[O, O], zr=I, ng=O);

                // 1
                assert_sim!(sys,
                    x=&[I, I], y=&[I, I], zx=I, nx=I, zy=I, ny=I, f=I, no=I
                    => out=[I, O], zr=O, ng=O);

                // -1
                assert_sim!(sys,
                    x=&[I, I], y=&[I, I], zx=I, nx=I, zy=I, ny=O, f=I, no=O
                    => out=[I, I], zr=O, ng=I);

                // x
                assert_sim!(sys,
                    x=&[I, O], y=&[O, I], zx=O, nx=O, zy=I, ny=I, f=O, no=O
                    => out=[I, O], zr=O, ng=O);

                // y
                assert_sim!(sys,
                    x=&[I, O], y=&[O, I], zx=I, nx=I, zy=O, ny=O, f=O, no=O
                    => out=[O, I], zr=O, ng=I);

                // !x
                assert_sim!(sys,
                    x=&[O, I], y=&[I, I], zx=O, nx=O, zy=I, ny=I, f=O, no=I
                    => out=[I, O], zr=O, ng=O);

                // !y
                assert_sim!(sys,
                    x=&[I, I], y=&[O, I], zx=I, nx=I, zy=O, ny=O, f=O, no=I
                    => out=[I, O], zr=O, ng=O);

                // -x
                assert_sim!(sys,
                    x=&[I, O], y=&[I, I], zx=O, nx=O, zy=I, ny=I, f=I, no=I
                    => out=[I, I], zr=O, ng=I);

                // -y
                assert_sim!(sys,
                    x=&[O, O], y=&[I, I], zx=I, nx=I, zy=O, ny=O, f=I, no=I
                    => out=[I, O], zr=O, ng=O);

                // x+1
                assert_sim!(sys,
                    x=&[O, O], y=&[O, O], zx=O, nx=I, zy=I, ny=I, f=I, no=I
                    => out=[I, O], zr=O, ng=O);

                // y+1
                assert_sim!(sys,
                    x=&[O, O], y=&[O, O], zx=I, nx=I, zy=O, ny=I, f=I, no=I
                    => out=[I, O], zr=O, ng=O);

                // x-1
                assert_sim!(sys,
                    x=&[O, O], y=&[I, O], zx=O, nx=O, zy=I, ny=I, f=I, no=O
                    => out=[I, I], zr=O, ng=I);

                // y-1
                assert_sim!(sys,
                    x=&[I, O], y=&[O, O], zx=I, nx=I, zy=O, ny=O, f=I, no=O
                    => out=[I, I], zr=O, ng=I);

                // x+y
                assert_sim!(sys,
                    x=&[I, O], y=&[I, O], zx=O, nx=O, zy=O, ny=O, f=I, no=O
                    => out=[O, I], zr=O, ng=I);

                // x-y
                assert_sim!(sys,
                    x=&[I, O], y=&[I, O], zx=O, nx=I, zy=O, ny=O, f=I, no=I
                    => out=[O, O], zr=I, ng=O);

                // y-x
                assert_sim!(sys,
                    x=&[I, O], y=&[I, I], zx=O, nx=O, zy=O, ny=I, f=I, no=I
                    => out=[O, I], zr=O, ng=I);

                // x&y
                assert_sim!(sys,
                    x=&[I, O], y=&[I, I], zx=O, nx=O, zy=O, ny=O, f=O, no=O
                    => out=[I, O], zr=O, ng=O);

                // x|y
                assert_sim!(sys,
                    x=&[I, O], y=&[O, O], zx=O, nx=I, zy=O, ny=I, f=O, no=I
                    => out=[I, O], zr=O, ng=O);
            }
        }
    }

    #[test]
    fn test_set_zero() {
        system! {
            sys
            wires { sel }
            buses { a[4], o[4] }
            gates {
                set_zero("zero", a, sel, o);
            }
            body {
                assert_sim!(sys, a=&[O, I, O, I], sel=O => o=&[O, I, O, I]);
                assert_sim!(sys, a=&[O, I, O, I], sel=I => o=&[O, O, O, O]);
            }
        }
    }

    #[test]
    fn test_negate() {
        system! {
            sys
            wires { sel }
            buses { a[4], o[4] }
            gates {
                negate("negate", a, sel, o);
            }
            body {
                assert_sim!(sys, a=&[O, I, O, I], sel=O => o=&[O, I, O, I]);
                assert_sim!(sys, a=&[O, I, O, I], sel=I => o=&[I, O, I, O]);
            }
        }
    }
}
