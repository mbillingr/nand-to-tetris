use std::fmt::{Display, Formatter};
use crate::chapter06_assembler::parser::FromStrNocopy;
use crate::chapter07_vm::parser::Command as StackCmd;

#[derive(Debug, Eq, PartialEq)]
enum Command<'s> {
    Stack(StackCmd),
    Label(&'s str),
    Goto(&'s str),
    IfGoto(&'s str),
    Function(&'s str, u16),
    Call(&'s str, u16),
    Return,
}

impl<'s> FromStrNocopy<'s> for Command<'s> {
    fn from_str(s: &'s str) -> Result<Self, String> {
        if let Ok(sc) = StackCmd::from_str(s) {
            return Ok(Command::Stack(sc));
        }

        let tokens: Vec<_> = s.split_whitespace().collect();
        match tokens.as_slice() {
            ["label", label] => Some(Command::Label(label)),
            ["goto", label] => Some(Command::Goto(label)),
            ["if-goto", label] => Some(Command::IfGoto(label)),
            ["function", name, n_vars] => n_vars.parse().map(|n| Command::Function(name, n)).ok(),
            ["call", name, n_vars] => n_vars.parse().map(|n| Command::Call(name, n)).ok(),
            ["return"] => Some(Command::Return),
            _ => None,
        }
        .ok_or_else(|| format!("Invalid instruction {}", tokens.join(" ")))
    }
}

impl Display for Command<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::Stack(sc) => write!(f, "{}", sc),
            Command::Label(label) => write!(f, "label {}", label),
            Command::Goto(label) => write!(f, "goto {}", label),
            Command::IfGoto(label) => write!(f, "if-goto {}", label),
            Command::Function(name, n) => write!(f, "function {} {}", name, n),
            Command::Call(name, n) => write!(f, "call {} {}", name, n),
            Command::Return => write!(f, "return")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::chapter07_vm::parser::{ArithmeticCmd::*, Segment::*};
    use Command::*;
    use StackCmd::*;

    #[test]
    fn parse_stack_commands() {
        assert_eq!(Command::from_str("add"), Ok(Stack(Arithmetic(Add))));
        assert_eq!(Command::from_str("push this 42"), Ok(Stack(Push(This, 42))));
    }

    #[test]
    fn parse_branching_commands() {
        assert_eq!(Command::from_str("label foo"), Ok(Label("foo")));
        assert_eq!(Command::from_str("goto foo"), Ok(Goto("foo")));
        assert_eq!(Command::from_str("if-goto foo"), Ok(IfGoto("foo")));
    }

    #[test]
    fn parse_function_commands() {
        assert_eq!(Command::from_str("function foo 0"), Ok(Function("foo", 0)));
        assert_eq!(Command::from_str("call bar 1"), Ok(Call("bar", 1)));
        assert_eq!(Command::from_str("return"), Ok(Return));
    }
}
