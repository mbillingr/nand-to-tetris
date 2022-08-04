use crate::chapter07_vm::parser::Segment::{Constant, Pointer};
use crate::chapter07_vm::parser::{ArithmeticCmd, Command as StackCmd};
use crate::chapter08_vm::parser::Command;
use crate::chapter10_parser::parser::Term;

pub struct Compiler<'s> {
    code: Vec<Command<'s>>,
}

impl<'s> Compiler<'s> {
    pub fn new() -> Self {
        Compiler { code: vec![] }
    }

    pub fn code(&self) -> &[Command] {
        &self.code
    }

    //fn define_local(&mut self, name: &'s str)

    fn compile_term(&mut self, term: Term) -> Result<(), String> {
        match term {
            Term::Null | Term::False => self.code.push(Command::Stack(StackCmd::Push(Constant, 0))),
            Term::True => self.code.extend([
                Command::Stack(StackCmd::Push(Constant, 1)),
                Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Neg)),
            ]),
            Term::This => self.code.push(Command::Stack(StackCmd::Push(Pointer, 0))),
            Term::Integer(x) => self.code.push(Command::Stack(StackCmd::Push(Constant, x))),
            Term::String(s) => {
                self.code.extend([
                    Command::Stack(StackCmd::Push(Constant, s.len() as u16)),
                    Command::Call("String.new", 1),
                ]);
                for ch in s.bytes() {
                    self.code
                        .push(Command::Stack(StackCmd::Push(Constant, ch as u16)));
                    self.code.push(Command::Call("String.appendChar", 1));
                }
            }
            Term::Variable(name) => return Err(format!("Undefined variable: {}", name)),
            _ => todo!(),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chapter07_vm::parser::ArithmeticCmd::Neg;
    use crate::chapter07_vm::parser::Command::{Arithmetic, Push};
    use crate::chapter07_vm::parser::Segment::Constant;
    use crate::chapter08_vm::parser::Command::{Call, Stack};
    use crate::chapter10_parser::parser::Type;

    macro_rules! term_tests {
        ($($name:ident: $term:expr => $expected:expr;)*) => {
            $(
                #[test]
                fn $name() {
                    let mut compiler = Compiler::new();
                    compiler.compile_term($term).unwrap();
                    assert_eq!(compiler.code(), $expected);
                }
            )*
        };
    }

    term_tests! {
        compile_null: Term::Null => [Stack(Push(Constant, 0))];
        compile_true: Term::True => [Stack(Push(Constant, 1)), Stack(Arithmetic(Neg))];
        compile_false: Term::False => [Stack(Push(Constant, 0))];
        compile_this: Term::This => [Stack(Push(Pointer, 0))];
        compile_int0: Term::Integer(0) => [Stack(Push(Constant, 0))];
        compile_int1: Term::Integer(1) => [Stack(Push(Constant, 1))];
        compile_str0: Term::string("") => [Stack(Push(Constant, 0)), Call("String.new", 1)];
        compile_str1: Term::string("a") => [Stack(Push(Constant, 1)), Call("String.new", 1), Stack(Push(Constant, 97)), Call("String.appendChar", 1)];
        compile_strx: Term::string("!*7=A_a~") => [
            Stack(Push(Constant, 8)), Call("String.new", 1),
            Stack(Push(Constant, 33)), Call("String.appendChar", 1),
            Stack(Push(Constant, 42)), Call("String.appendChar", 1),
            Stack(Push(Constant, 55)), Call("String.appendChar", 1),
            Stack(Push(Constant, 61)), Call("String.appendChar", 1),
            Stack(Push(Constant, 65)), Call("String.appendChar", 1),
            Stack(Push(Constant, 95)), Call("String.appendChar", 1),
            Stack(Push(Constant, 97)), Call("String.appendChar", 1),
            Stack(Push(Constant, 126)), Call("String.appendChar", 1),
        ];
    }

    #[test]
    fn compile_undefined_variable() {
        let mut compiler = Compiler::new();
        let err = compiler.compile_term(Term::variable("foo"));
        assert_eq!(err, Err("Undefined variable: foo".to_string()));
    }

    /*#[test]
    fn compile_local_variable() {
        let mut compiler = Compiler::new();
        compiler.define_local("foo", Type::Int);
        compiler.compile_term(Term::variable("foo")).unwrap();
        assert_eq!(compiler.code(), []);
    }*/
}
