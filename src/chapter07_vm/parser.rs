use crate::chapter06_assembler::parser::FromStrNocopy;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub enum Command {
    Arithmetic(ArithmeticCmd),
    Push(Segment, u16),
    Pop(Segment, u16),
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

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::Arithmetic(ac) => write!(f, "{}", ac),
            Command::Push(seg, idx) => write!(f, "push {} {}", seg, idx),
            Command::Pop(seg, idx) => write!(f, "pop {} {}", seg, idx),
        }
    }
}

impl Command {
    pub fn try_from_char(ch: char) -> Option<Self> {
        match ch {
            '0' => Some(Command::Push(Segment::Constant, 0)),
            '1' => Some(Command::Push(Segment::Constant, 1)),
            '2' => Some(Command::Push(Segment::Constant, 2)),
            '3' => Some(Command::Push(Segment::Constant, 3)),
            '4' => Some(Command::Push(Segment::Constant, 4)),
            '5' => Some(Command::Push(Segment::Constant, 5)),
            '6' => Some(Command::Push(Segment::Constant, 6)),
            '7' => Some(Command::Push(Segment::Constant, 7)),
            '8' => Some(Command::Push(Segment::Constant, 8)),
            '9' => Some(Command::Push(Segment::Constant, 9)),
            _ => ArithmeticCmd::try_from_char(ch).map(Command::Arithmetic),
        }
    }
}

impl Display for ArithmeticCmd {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use ArithmeticCmd::*;
        match self {
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            Neg => write!(f, "neg"),
            Eq => write!(f, "eq"),
            Gt => write!(f, "gt"),
            Lt => write!(f, "lt"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Not => write!(f, "not"),
        }
    }
}

impl ArithmeticCmd {
    pub fn try_from_char(ch: char) -> Option<Self> {
        match ch {
            '+' => Some(ArithmeticCmd::Add),
            '-' => Some(ArithmeticCmd::Sub),
            '=' => Some(ArithmeticCmd::Eq),
            '>' => Some(ArithmeticCmd::Gt),
            '<' => Some(ArithmeticCmd::Lt),
            '&' => Some(ArithmeticCmd::And),
            '|' => Some(ArithmeticCmd::Or),
            '~' => Some(ArithmeticCmd::Not),
            _ => None,
        }
    }
}

impl Display for Segment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Segment::Argument => write!(f, "argument"),
            Segment::Local => write!(f, "local"),
            Segment::Static => write!(f, "static"),
            Segment::Constant => write!(f, "constant"),
            Segment::This => write!(f, "this"),
            Segment::That => write!(f, "that"),
            Segment::Pointer => write!(f, "pointer"),
            Segment::Temp => write!(f, "temp"),
        }
    }
}

impl Segment {
    pub fn register_name(&self) -> Option<&'static str> {
        match self {
            Segment::Argument => Some("ARG"),
            Segment::Local => Some("LCL"),
            Segment::This => Some("THIS"),
            Segment::That => Some("THAT"),
            _ => None,
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
