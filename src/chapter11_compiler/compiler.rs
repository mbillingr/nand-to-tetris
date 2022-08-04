use crate::chapter07_vm::parser::Segment::{Constant, Pointer};
use crate::chapter07_vm::parser::{ArithmeticCmd, Command as StackCmd, Segment};
use crate::chapter08_vm::parser::Command;
use crate::chapter10_parser::parser::{Term, Type};
use crate::chapter11_compiler::symbol_table::{self, SymbolTable, VarKind};

pub struct Compiler<'s> {
    code: Vec<Command<'s>>,
    function_symbols: SymbolTable<'s>,
    class_symbols: SymbolTable<'s>,
}

impl<'s> Compiler<'s> {
    pub fn new() -> Self {
        Compiler {
            code: vec![],
            function_symbols: SymbolTable::new(),
            class_symbols: SymbolTable::new(),
        }
    }

    pub fn code(&self) -> &[Command] {
        &self.code
    }

    fn define_local(&mut self, name: &'s str, typ: Type<'s>) {
        self.function_symbols.define(name, typ, VarKind::Var);
    }

    fn define_arg(&mut self, name: &'s str, typ: Type<'s>) {
        self.function_symbols.define(name, typ, VarKind::Arg);
    }

    fn define_static(&mut self, name: &'s str, typ: Type<'s>) {
        self.class_symbols.define(name, typ, VarKind::Static);
    }

    fn define_field(&mut self, name: &'s str, typ: Type<'s>) {
        self.class_symbols.define(name, typ, VarKind::Field);
    }

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
            Term::Variable(name) => {
                let lookup = self
                    .function_symbols
                    .find(name)
                    .or_else(|| self.class_symbols.find(name));
                match lookup {
                    Some(symbol_table::Entry { typ, kind, index }) => self
                        .code
                        .push(Command::Stack(StackCmd::Push(kind.into(), *index))),
                    None => return Err(format!("Undefined variable: {}", name)),
                }
            }
            Term::ArrayIndex(_, _) => todo!(),
            Term::Expression(_) => todo!(),
            Term::Neg(term) => {
                self.compile_term(*term);
                self.code.push(Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Neg)));
            }
            Term::Not(term) => {
                self.compile_term(*term);
                self.code.push(Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Not)));
            }

            Term::Call(_) => todo!(),
        }
        Ok(())
    }
}

impl From<&VarKind> for Segment {
    fn from(kind: &VarKind) -> Self {
        match kind {
            VarKind::Static => Segment::Static,
            VarKind::Field => Segment::This,
            VarKind::Var => Segment::Local,
            VarKind::Arg => Segment::Argument,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chapter07_vm::parser::ArithmeticCmd::{Neg, Not};
    use crate::chapter07_vm::parser::Command::{Arithmetic, Push};
    use crate::chapter07_vm::parser::Segment::{Argument, Constant, Local, Static, This};
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
        compile_neg: Term::neg(Term::integer(2)) => [Stack(Push(Constant, 2)), Stack(Arithmetic(Neg))];
        compile_not: Term::not(Term::False) => [Stack(Push(Constant, 0)), Stack(Arithmetic(Not))];
    }

    #[test]
    fn compile_undefined_variable() {
        let mut compiler = Compiler::new();
        let err = compiler.compile_term(Term::variable("foo"));
        assert_eq!(err, Err("Undefined variable: foo".to_string()));
    }

    #[test]
    fn compile_local_variable_reference() {
        let mut compiler = Compiler::new();
        compiler.define_local("foo", Type::Int);
        compiler.compile_term(Term::variable("foo")).unwrap();
        assert_eq!(compiler.code(), [Stack(Push(Local, 0))]);
    }

    #[test]
    fn compile_argument_reference() {
        let mut compiler = Compiler::new();
        compiler.define_arg("foo", Type::Int);
        compiler.compile_term(Term::variable("foo")).unwrap();
        assert_eq!(compiler.code(), [Stack(Push(Argument, 0))]);
    }

    #[test]
    fn compile_static_reference() {
        let mut compiler = Compiler::new();
        compiler.define_static("foo", Type::Int);
        compiler.compile_term(Term::variable("foo")).unwrap();
        assert_eq!(compiler.code(), [Stack(Push(Static, 0))]);
    }

    #[test]
    fn compile_field_reference() {
        let mut compiler = Compiler::new();
        compiler.define_field("foo", Type::Int);
        compiler.compile_term(Term::variable("foo")).unwrap();
        assert_eq!(compiler.code(), [Stack(Push(This, 0))]);
    }
}
