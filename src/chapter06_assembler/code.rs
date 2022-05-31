use crate::chapter06_assembler::parser::{Comp, Dest, Jump};

impl Dest {
    pub fn code(&self) -> &'static str {
        match self {
            Dest::None => "000",
            Dest::M => "001",
            Dest::D => "010",
            Dest::DM => "011",
            Dest::A => "100",
            Dest::AM => "101",
            Dest::AD => "110",
            Dest::ADM => "111",
        }
    }
}

impl Jump {
    pub fn code(&self) -> &'static str {
        match self {
            Jump::None => "000",
            Jump::GreaterZero => "001",
            Jump::EqualZero => "010",
            Jump::NotNegative => "011",
            Jump::Negative => "100",
            Jump::NotZero => "101",
            Jump::NegativeOrZero => "110",
            Jump::Unconditional => "111",
        }
    }
}

impl Comp {
    pub fn code(&self) -> &'static str {
        match self {
            Comp::Zero => "0101010",
            Comp::One => "0111111",
            Comp::MinusOne => "0111010",
            Comp::D => "0001100",
            Comp::A => "0110000",
            Comp::M => "1110000",
            Comp::NotD => "0001101",
            Comp::NotA => "0110001",
            Comp::NotM => "1110001",
            Comp::NegD => "0001111",
            Comp::NegA => "0110011",
            Comp::NegM => "1110011",
            Comp::IncD => "0011111",
            Comp::IncA => "0110111",
            Comp::IncM => "1110111",
            Comp::DecD => "0001110",
            Comp::DecA => "0110010",
            Comp::DecM => "1110010",
            Comp::DplusA => "0000010",
            Comp::DplusM => "1000010",
            Comp::DminusA => "0010011",
            Comp::DminusM => "1010011",
            Comp::AminusD => "0000111",
            Comp::MminusD => "1000111",
            Comp::DandA => "0000000",
            Comp::DandM => "1000000",
            Comp::DorA => "0010101",
            Comp::DorM => "0010101",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dest_mnemonics() {
        assert_eq!(Dest::None.code(), "000");
        assert_eq!(Dest::M.code(), "001");
        assert_eq!(Dest::D.code(), "010");
        assert_eq!(Dest::DM.code(), "011");
        assert_eq!(Dest::A.code(), "100");
        assert_eq!(Dest::AM.code(), "101");
        assert_eq!(Dest::AD.code(), "110");
        assert_eq!(Dest::ADM.code(), "111");
    }

    #[test]
    fn jump_mnemonics() {
        assert_eq!(Jump::None.code(), "000");
        assert_eq!(Jump::GreaterZero.code(), "001");
        assert_eq!(Jump::EqualZero.code(), "010");
        assert_eq!(Jump::NotNegative.code(), "011");
        assert_eq!(Jump::Negative.code(), "100");
        assert_eq!(Jump::NotZero.code(), "101");
        assert_eq!(Jump::NegativeOrZero.code(), "110");
        assert_eq!(Jump::Unconditional.code(), "111");
    }

    #[test]
    fn comp_mnemonics() {
        assert_eq!(Comp::Zero.code(), "0101010");
        assert_eq!(Comp::One.code(), "0111111");
        assert_eq!(Comp::MinusOne.code(), "0111010");
        assert_eq!(Comp::D.code(), "0001100");
        assert_eq!(Comp::A.code(), "0110000");
        assert_eq!(Comp::M.code(), "1110000");
        assert_eq!(Comp::NotD.code(), "0001101");
        assert_eq!(Comp::NotA.code(), "0110001");
        assert_eq!(Comp::NotM.code(), "1110001");
        assert_eq!(Comp::NegD.code(), "0001111");
        assert_eq!(Comp::NegA.code(), "0110011");
        assert_eq!(Comp::NegM.code(), "1110011");
        assert_eq!(Comp::IncD.code(), "0011111");
        assert_eq!(Comp::IncA.code(), "0110111");
        assert_eq!(Comp::IncM.code(), "1110111");
        assert_eq!(Comp::DecD.code(), "0001110");
        assert_eq!(Comp::DecA.code(), "0110010");
        assert_eq!(Comp::DecM.code(), "1110010");
        assert_eq!(Comp::DplusA.code(), "0000010");
        assert_eq!(Comp::DplusM.code(), "1000010");
        assert_eq!(Comp::DminusA.code(), "0010011");
        assert_eq!(Comp::DminusM.code(), "1010011");
        assert_eq!(Comp::AminusD.code(), "0000111");
        assert_eq!(Comp::MminusD.code(), "1000111");
        assert_eq!(Comp::DandA.code(), "0000000");
        assert_eq!(Comp::DandM.code(), "1000000");
        assert_eq!(Comp::DorA.code(), "0010101");
        assert_eq!(Comp::DorM.code(), "0010101");
    }
}
