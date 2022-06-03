use crate::chapter06_assembler::parser::FromStrNocopy;
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub enum Command {
    Arithmetic(ArithmeticCmd),
    Push(Segment, usize),
    Pop(Segment, usize),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ArithmeticCmd {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Segment {
    Argument,
    Local,
    Static,
    Constant,
    This,
    That,
    Pointer,
    Temp,
}

impl Segment {
    pub fn is_special(&self) -> bool {
        match self {
            Segment::Argument | Segment::Local | Segment::This | Segment::That => false,
            Segment::Constant => true,
            _ => todo!(),
        }
    }
}

impl<'s> FromStrNocopy<'s> for Command {
    fn from_str(s: &'s str) -> Result<Self, String> {
        if let Ok(ac) = ArithmeticCmd::from_str(s) {
            return Ok(Command::Arithmetic(ac));
        }

        let tokens: Vec<_> = s.split_whitespace().collect();
        match tokens.as_slice() {
            ["push", segment, index] => Ok(Command::Push(
                Segment::from_str(segment)?,
                FromStrNocopy::from_str(index)?,
            )),
            ["pop", segment, index] => Ok(Command::Pop(
                Segment::from_str(segment)?,
                FromStrNocopy::from_str(index)?,
            )),
            _ => Err(format!("Invalid instruction {}", tokens.join(" "))),
        }
    }
}

impl FromStr for ArithmeticCmd {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        match s {
            "add" => Ok(ArithmeticCmd::Add),
            "sub" => Ok(ArithmeticCmd::Sub),
            "neg" => Ok(ArithmeticCmd::Neg),
            "eq" => Ok(ArithmeticCmd::Eq),
            "gt" => Ok(ArithmeticCmd::Gt),
            "lt" => Ok(ArithmeticCmd::Lt),
            "and" => Ok(ArithmeticCmd::And),
            "or" => Ok(ArithmeticCmd::Or),
            "not" => Ok(ArithmeticCmd::Not),
            _ => Err(()),
        }
    }
}

impl<'s> FromStrNocopy<'s> for Segment {
    fn from_str(s: &'s str) -> Result<Self, String> {
        match s {
            "argument" => Ok(Segment::Argument),
            "local" => Ok(Segment::Local),
            "static" => Ok(Segment::Static),
            "constant" => Ok(Segment::Constant),
            "this" => Ok(Segment::This),
            "that" => Ok(Segment::That),
            "pointer" => Ok(Segment::Pointer),
            "temp" => Ok(Segment::Temp),
            _ => Err(format!("Invalid segment {}", s)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ArithmeticCmd::*;
    use Command::*;
    use Segment::*;

    #[test]
    fn parse_arithmetic() {
        assert_eq!(ArithmeticCmd::from_str("add"), Ok(Add));
        assert_eq!(ArithmeticCmd::from_str("sub"), Ok(Sub));
        assert_eq!(ArithmeticCmd::from_str("neg"), Ok(Neg));
        assert_eq!(ArithmeticCmd::from_str("eq"), Ok(Eq));
        assert_eq!(ArithmeticCmd::from_str("gt"), Ok(Gt));
        assert_eq!(ArithmeticCmd::from_str("lt"), Ok(Lt));
        assert_eq!(ArithmeticCmd::from_str("and"), Ok(And));
        assert_eq!(ArithmeticCmd::from_str("or"), Ok(Or));
        assert_eq!(ArithmeticCmd::from_str("not"), Ok(Not));
    }

    #[test]
    fn parse_segment() {
        assert_eq!(Segment::from_str("argument"), Ok(Argument));
        assert_eq!(Segment::from_str("local"), Ok(Local));
        assert_eq!(Segment::from_str("static"), Ok(Static));
        assert_eq!(Segment::from_str("constant"), Ok(Constant));
        assert_eq!(Segment::from_str("this"), Ok(This));
        assert_eq!(Segment::from_str("that"), Ok(That));
        assert_eq!(Segment::from_str("pointer"), Ok(Pointer));
        assert_eq!(Segment::from_str("temp"), Ok(Temp));
    }

    #[test]
    fn parse_arithmetic_command() {
        assert_eq!(Command::from_str("add"), Ok(Arithmetic(Add)));
    }

    #[test]
    fn parse_push_command() {
        assert_eq!(Command::from_str("push this 0"), Ok(Push(This, 0)));
        assert_eq!(Command::from_str("push that 1"), Ok(Push(That, 1)));
    }

    #[test]
    fn parse_pop_command() {
        assert_eq!(Command::from_str("pop this 0"), Ok(Pop(This, 0)));
        assert_eq!(Command::from_str("pop that 1"), Ok(Pop(That, 1)));
    }
}
