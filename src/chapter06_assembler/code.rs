use crate::chapter06_assembler::parser::{Comp, Dest, Jump};

impl Dest {
    pub fn code(&self) -> u16 {
        match self {
            Dest::None => 0b000,
            Dest::M => 0b001,
            Dest::D => 0b010,
            Dest::DM => 0b011,
            Dest::A => 0b100,
            Dest::AM => 0b101,
            Dest::AD => 0b110,
            Dest::ADM => 0b111,
        }
    }
}

impl Jump {
    pub fn code(&self) -> u16 {
        match self {
            Jump::None => 0b000,
            Jump::GreaterZero => 0b001,
            Jump::EqualZero => 0b010,
            Jump::NotNegative => 0b011,
            Jump::Negative => 0b100,
            Jump::NotZero => 0b101,
            Jump::NegativeOrZero => 0b110,
            Jump::Unconditional => 0b111,
        }
    }
}

impl Comp {
    pub fn code(&self) -> u16 {
        match self {
            Comp::Zero => 0b0101010,
            Comp::One => 0b0111111,
            Comp::MinusOne => 0b0111010,
            Comp::D => 0b0001100,
            Comp::A => 0b0110000,
            Comp::M => 0b1110000,
            Comp::NotD => 0b0001101,
            Comp::NotA => 0b0110001,
            Comp::NotM => 0b1110001,
            Comp::NegD => 0b0001111,
            Comp::NegA => 0b0110011,
            Comp::NegM => 0b1110011,
            Comp::IncD => 0b0011111,
            Comp::IncA => 0b0110111,
            Comp::IncM => 0b1110111,
            Comp::DecD => 0b0001110,
            Comp::DecA => 0b0110010,
            Comp::DecM => 0b1110010,
            Comp::DplusA => 0b0000010,
            Comp::DplusM => 0b1000010,
            Comp::DminusA => 0b0010011,
            Comp::DminusM => 0b1010011,
            Comp::AminusD => 0b0000111,
            Comp::MminusD => 0b1000111,
            Comp::DandA => 0b0000000,
            Comp::DandM => 0b1000000,
            Comp::DorA => 0b0010101,
            Comp::DorM => 0b0010101,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dest_mnemonics() {
        assert_eq!(Dest::None.code(), 0b000);
        assert_eq!(Dest::M.code(), 0b001);
        assert_eq!(Dest::D.code(), 0b010);
        assert_eq!(Dest::DM.code(), 0b011);
        assert_eq!(Dest::A.code(), 0b100);
        assert_eq!(Dest::AM.code(), 0b101);
        assert_eq!(Dest::AD.code(), 0b110);
        assert_eq!(Dest::ADM.code(), 0b111);
    }

    #[test]
    fn jump_mnemonics() {
        assert_eq!(Jump::None.code(), 0b000);
        assert_eq!(Jump::GreaterZero.code(), 0b001);
        assert_eq!(Jump::EqualZero.code(), 0b010);
        assert_eq!(Jump::NotNegative.code(), 0b011);
        assert_eq!(Jump::Negative.code(), 0b100);
        assert_eq!(Jump::NotZero.code(), 0b101);
        assert_eq!(Jump::NegativeOrZero.code(), 0b110);
        assert_eq!(Jump::Unconditional.code(), 0b111);
    }

    #[test]
    fn comp_mnemonics() {
        assert_eq!(Comp::Zero.code(), 0b0101010);
        assert_eq!(Comp::One.code(), 0b0111111);
        assert_eq!(Comp::MinusOne.code(), 0b0111010);
        assert_eq!(Comp::D.code(), 0b0001100);
        assert_eq!(Comp::A.code(), 0b0110000);
        assert_eq!(Comp::M.code(), 0b1110000);
        assert_eq!(Comp::NotD.code(), 0b0001101);
        assert_eq!(Comp::NotA.code(), 0b0110001);
        assert_eq!(Comp::NotM.code(), 0b1110001);
        assert_eq!(Comp::NegD.code(), 0b0001111);
        assert_eq!(Comp::NegA.code(), 0b0110011);
        assert_eq!(Comp::NegM.code(), 0b1110011);
        assert_eq!(Comp::IncD.code(), 0b0011111);
        assert_eq!(Comp::IncA.code(), 0b0110111);
        assert_eq!(Comp::IncM.code(), 0b1110111);
        assert_eq!(Comp::DecD.code(), 0b0001110);
        assert_eq!(Comp::DecA.code(), 0b0110010);
        assert_eq!(Comp::DecM.code(), 0b1110010);
        assert_eq!(Comp::DplusA.code(), 0b0000010);
        assert_eq!(Comp::DplusM.code(), 0b1000010);
        assert_eq!(Comp::DminusA.code(), 0b0010011);
        assert_eq!(Comp::DminusM.code(), 0b1010011);
        assert_eq!(Comp::AminusD.code(), 0b0000111);
        assert_eq!(Comp::MminusD.code(), 0b1000111);
        assert_eq!(Comp::DandA.code(), 0b0000000);
        assert_eq!(Comp::DandM.code(), 0b1000000);
        assert_eq!(Comp::DorA.code(), 0b0010101);
        assert_eq!(Comp::DorM.code(), 0b0010101);
    }
}
