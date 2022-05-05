use Bool::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Bool {
    O,
    I,
}

pub fn nand(a: Bool, b: Bool) -> Bool {
    match (a, b) {
        (I, I) => O,
        _ => I,
    }
}

pub fn not(a: Bool) -> Bool {
    nand(a, a)
}

pub fn and(a: Bool, b: Bool) -> Bool {
    not(nand(a, b))
}

pub fn or(a: Bool, b: Bool) -> Bool {
    nand(not(a), not(b))
}

pub fn xor(a: Bool, b: Bool) -> Bool {
    nand(nand(a, not(b)), nand(not(a), b))
}

pub fn mux(a: Bool, b: Bool, sel: Bool) -> Bool {
    nand(nand(sel, b), nand(not(sel), a))
}

pub fn demux(i: Bool, sel: Bool) -> (Bool, Bool) {
    let a = and(i, not(sel));
    let b = and(i, sel);
    (a, b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nand_gate() {
        assert_eq!(nand(O, O), I);
        assert_eq!(nand(O, I), I);
        assert_eq!(nand(I, O), I);
        assert_eq!(nand(I, I), O);
    }

    #[test]
    fn not_gate() {
        assert_eq!(not(O), I);
        assert_eq!(not(I), O);
    }

    #[test]
    fn and_gate() {
        assert_eq!(and(O, O), O);
        assert_eq!(and(O, I), O);
        assert_eq!(and(I, O), O);
        assert_eq!(and(I, I), I);
    }

    #[test]
    fn or_gate() {
        assert_eq!(or(O, O), O);
        assert_eq!(or(O, I), I);
        assert_eq!(or(I, O), I);
        assert_eq!(or(I, I), I);
    }

    #[test]
    fn xor_gate() {
        assert_eq!(xor(O, O), O);
        assert_eq!(xor(O, I), I);
        assert_eq!(xor(I, O), I);
        assert_eq!(xor(I, I), O);
    }

    #[test]
    fn mux_gate() {
        assert_eq!(mux(O, O, O), O);
        assert_eq!(mux(O, I, O), O);
        assert_eq!(mux(I, O, O), I);
        assert_eq!(mux(I, I, O), I);
        assert_eq!(mux(O, O, I), O);
        assert_eq!(mux(O, I, I), I);
        assert_eq!(mux(I, O, I), O);
        assert_eq!(mux(I, I, I), I);
    }

    #[test]
    fn demux_gate() {
        assert_eq!(demux(O, O), (O, O));
        assert_eq!(demux(O, I), (O, O));
        assert_eq!(demux(I, O), (I, O));
        assert_eq!(demux(I, I), (O, I));
    }
}
