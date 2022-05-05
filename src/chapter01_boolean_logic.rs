use Bool::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Bool {
    O,
    I,
}

pub trait Logic {
    fn nand(a: &Self, b: &Self) -> Self;
}

impl Logic for Bool {
    fn nand(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (I, I) => O,
            _ => I,
        }
    }
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

pub fn mux<T: Logic>(a: &T, b: &T, sel: &T) -> T {
    nand(&nand(sel, b), &nand(&not(sel), a))
}

pub fn demux<T: Logic>(i: &T, sel: &T) -> (T, T) {
    let a = and(i, &not(sel));
    let b = and(i, sel);
    (a, b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nand_gate() {
        assert_eq!(nand(&O, &O), I);
        assert_eq!(nand(&O, &I), I);
        assert_eq!(nand(&I, &O), I);
        assert_eq!(nand(&I, &I), O);
    }

    #[test]
    fn not_gate() {
        assert_eq!(not(&O), I);
        assert_eq!(not(&I), O);
    }

    #[test]
    fn and_gate() {
        assert_eq!(and(&O, &O), O);
        assert_eq!(and(&O, &I), O);
        assert_eq!(and(&I, &O), O);
        assert_eq!(and(&I, &I), I);
    }

    #[test]
    fn or_gate() {
        assert_eq!(or(&O, &O), O);
        assert_eq!(or(&O, &I), I);
        assert_eq!(or(&I, &O), I);
        assert_eq!(or(&I, &I), I);
    }

    #[test]
    fn xor_gate() {
        assert_eq!(xor(&O, &O), O);
        assert_eq!(xor(&O, &I), I);
        assert_eq!(xor(&I, &O), I);
        assert_eq!(xor(&I, &I), O);
    }

    #[test]
    fn mux_gate() {
        assert_eq!(mux(&O, &O, &O), O);
        assert_eq!(mux(&O, &I, &O), O);
        assert_eq!(mux(&I, &O, &O), I);
        assert_eq!(mux(&I, &I, &O), I);
        assert_eq!(mux(&O, &O, &I), O);
        assert_eq!(mux(&O, &I, &I), I);
        assert_eq!(mux(&I, &O, &I), O);
        assert_eq!(mux(&I, &I, &I), I);
    }

    #[test]
    fn demux_gate() {
        assert_eq!(demux(&O, &O), (O, O));
        assert_eq!(demux(&O, &I), (O, O));
        assert_eq!(demux(&I, &O), (I, O));
        assert_eq!(demux(&I, &I), (O, I));
    }

    /*#[test]
    fn multi_bit_not() {
        assert_eq!()
    }*/
}
