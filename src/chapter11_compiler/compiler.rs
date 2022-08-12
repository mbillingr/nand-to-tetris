use crate::chapter07_vm::parser::Segment::{Constant, Pointer};
use crate::chapter07_vm::parser::{ArithmeticCmd, Command as StackCmd, Segment};
use crate::chapter08_vm::parser::Command;
use crate::chapter10_parser::parser::{Expression, Statement, SubroutineCall, Term, Type};
use crate::chapter11_compiler::symbol_table::{self, Entry, SymbolTable, VarKind};

pub struct Compiler<'s> {
    code: Vec<Command<'s>>,
    function_symbols: SymbolTable<'s>,
    class_symbols: SymbolTable<'s>,
    label_counter: usize,
}

impl<'s> Compiler<'s> {
    pub fn new() -> Self {
        Compiler {
            code: vec![],
            function_symbols: SymbolTable::new(),
            class_symbols: SymbolTable::new(),
            label_counter: 0,
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

    fn unique_label(&mut self, name: &str) -> &'s str {
        self.label_counter += 1;
        let label = format!("{}-{}", name, self.label_counter);
        // Rather evilly leak the string in order to create a name with a lifetime of at least 's.
        return Box::leak(label.into_boxed_str());
    }

    fn compile_statement(&mut self, stmt: Statement<'s>) -> Result<(), String> {
        match stmt {
            Statement::Return(None) => {
                self.compile_statement(Statement::Return(Some(Expression::Term(Term::Null))))?
            }
            Statement::Return(Some(expr)) => {
                self.compile_expression(expr)?;
                self.code.push(Command::Return);
            }
            Statement::Let(var, None, expr) => {
                self.compile_expression(expr)?;
                self.compile_varible_assignment(var)?;
            }
            Statement::Let(var, Some(idx), expr) => {
                self.compile_expression(expr)?;
                self.compile_varible_reference(var)?;
                self.compile_expression(idx)?;
                self.code
                    .push(Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Add)));
                self.code
                    .push(Command::Stack(StackCmd::Pop(Segment::Pointer, 1)));
                self.code
                    .push(Command::Stack(StackCmd::Pop(Segment::That, 0)));
            }
            Statement::Do(call) => {
                self.compile_term(Term::Call(call))?;
                self.code
                    .push(Command::Stack(StackCmd::Pop(Segment::Temp, 0)));
            }
            Statement::If(cond, yes, no) => {
                let l1 = self.unique_label("IF-TRUE");
                let l2 = self.unique_label("IF-END");
                self.compile_expression(cond)?;
                self.code.push(Command::IfGoto(l1));
                self.compile_block(no)?;
                self.code.push(Command::Goto(l2));
                self.code.push(Command::Label(l1));
                self.compile_block(yes)?;
                self.code.push(Command::Label(l2));
            }
            Statement::While(cond, body) => {
                let l1 = self.unique_label("WHILE");
                let l2 = self.unique_label("WHILE-END");
                self.code.push(Command::Label(l1));
                self.compile_expression(cond)?;
                self.code
                    .push(Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Not)));
                self.code.push(Command::IfGoto(l2));
                self.compile_block(body)?;
                self.code.push(Command::Goto(l1));
                self.code.push(Command::Label(l2));
            }
        }
        Ok(())
    }

    fn compile_block(&mut self, stmts: Vec<Statement<'s>>) -> Result<(), String> {
        for stmt in stmts {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn compile_expression(&mut self, expr: Expression<'s>) -> Result<(), String> {
        match expr {
            Expression::Term(term) => self.compile_term(term),
            Expression::Op(a, op, b) => {
                self.compile_term(a)?;
                self.compile_expression(*b)?;
                match op {
                    '*' => self.code.push(Command::Call("Math.multiply", 2)),
                    '/' => self.code.push(Command::Call("Math.divide", 2)),
                    _ => self
                        .code
                        .push(Command::try_from_char(op).expect("invalid op")),
                }
                Ok(())
            }
        }
    }

    fn compile_term(&mut self, term: Term<'s>) -> Result<(), String> {
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
                self.compile_varible_reference(name)?;
            }
            Term::ArrayIndex(var, idx) => {
                self.compile_varible_reference(var)?;
                self.compile_expression(*idx)?;
                self.code
                    .push(Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Add)));
                self.code
                    .push(Command::Stack(StackCmd::Pop(Segment::Pointer, 1)));
                self.code
                    .push(Command::Stack(StackCmd::Push(Segment::That, 0)));
            }
            Term::Expression(expr) => return self.compile_expression(*expr),
            Term::Neg(term) => {
                self.compile_term(*term)?;
                self.code
                    .push(Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Neg)));
            }
            Term::Not(term) => {
                self.compile_term(*term)?;
                self.code
                    .push(Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Not)));
            }

            Term::Call(SubroutineCall::FunctionCall(name, args)) => {
                let n_args = args.len() as u16;
                self.compile_args(args)?;
                self.code.push(Command::Call(name, n_args));
            }
            Term::Call(SubroutineCall::MethodCall(obj, name, args)) => {
                let n_args = 1 + args.len() as u16;
                match self.lookup(obj)? {
                    symbol_table::Entry {
                        typ: Type::Class(cls),
                        kind,
                        index,
                    } => {
                        // Rather evilly leak the string in order to create the fully qualified
                        // name with a lifetime that exceeds 's.
                        let full_name = Box::leak(format!("{cls}.{name}").into_boxed_str());
                        self.code
                            .push(Command::Stack(StackCmd::Push(kind.into(), *index)));
                        self.compile_args(args)?;
                        self.code.push(Command::Call(full_name, n_args));
                    }
                    symbol_table::Entry { typ, .. } => {
                        return Err(format!("Not an object: {:?}", typ))
                    }
                }
            }
        }
        Ok(())
    }

    fn compile_args(&mut self, args: Vec<Expression<'s>>) -> Result<(), String> {
        for arg in args {
            self.compile_expression(arg)?;
        }
        Ok(())
    }

    fn compile_varible_reference(&mut self, name: &str) -> Result<(), String> {
        let symbol_table::Entry {
            typ: _,
            kind,
            index,
        } = self.lookup(name)?;
        self.code
            .push(Command::Stack(StackCmd::Push(kind.into(), *index)));
        Ok(())
    }

    fn compile_varible_assignment(&mut self, name: &str) -> Result<(), String> {
        let symbol_table::Entry {
            typ: _,
            kind,
            index,
        } = self.lookup(name)?;
        self.code
            .push(Command::Stack(StackCmd::Pop(kind.into(), *index)));
        Ok(())
    }

    fn lookup(&self, name: &str) -> Result<&Entry<'s>, String> {
        self.function_symbols
            .find(name)
            .or_else(|| self.class_symbols.find(name))
            .ok_or_else(|| format!("Undefined variable: {}", name))
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
    use crate::chapter07_vm::parser::ArithmeticCmd::*;
    use crate::chapter07_vm::parser::Command::*;
    use crate::chapter07_vm::parser::Segment::*;
    use crate::chapter08_vm::parser::Command::*;
    use crate::chapter10_parser::parser::{Expression, Statement, SubroutineCall, Type};

    macro_rules! compiler_tests {
        ($f:ident: $($name:ident: $term:expr => $expected:expr;)*) => {
            $(
                #[test]
                fn $name() {
                    let mut compiler = Compiler::new();
                    compiler.$f($term).unwrap();
                    assert_eq!(compiler.code(), $expected);
                }
            )*
        };
    }

    compiler_tests! {
        compile_statement:
        compile_return: Statement::Return(None) => [Stack(Push(Constant, 0)), Return];
        compile_do: Statement::Do(SubroutineCall::FunctionCall("foo", vec![])) => [Call("foo", 0), Stack(Pop(Temp, 0))];
        compile_if: Statement::If(Term::False.into(), vec![Statement::Return(Some(1.into()))], vec![Statement::Return(Some(2.into()))]) => [
            Stack(Push(Constant, 0)),
            IfGoto("IF-TRUE-1"),
            Stack(Push(Constant, 2)),
            Return,
            Goto("IF-END-2"),
            Label("IF-TRUE-1"),
            Stack(Push(Constant, 1)),
            Return,
            Label("IF-END-2")
        ];
        compile_while: Statement::While(Term::False.into(), vec![Statement::Return(Some(1.into()))]) => [
            Label("WHILE-1"),
            Stack(Push(Constant, 0)),
            Stack(Arithmetic(Not)),
            IfGoto("WHILE-END-2"),
            Stack(Push(Constant, 1)),
            Return,
            Goto("WHILE-1"),
            Label("WHILE-END-2")
        ];
    }

    compiler_tests! {
        compile_expression:
        compile_simple_expression: Expression::Term(Term::Null) => [Stack(Push(Constant, 0))];
        compile_add: Expression::op('+', 1, 2) => [Stack(Push(Constant, 1)), Stack(Push(Constant, 2)), Stack(Arithmetic(Add))];
        compile_sub: Expression::op('-', 2, 1) => [Stack(Push(Constant, 2)), Stack(Push(Constant, 1)), Stack(Arithmetic(Sub))];
        compile_mul: Expression::op('*', 2, 3) => [Stack(Push(Constant, 2)), Stack(Push(Constant, 3)), Call("Math.multiply", 2)];
        compile_div: Expression::op('/', 4, 2) => [Stack(Push(Constant, 4)), Stack(Push(Constant, 2)), Call("Math.divide", 2)];
        compile_and: Expression::op('&', 5, 3) => [Stack(Push(Constant, 5)), Stack(Push(Constant, 3)), Stack(Arithmetic(And))];
        compile_or: Expression::op('|', 1, 2) => [Stack(Push(Constant, 1)), Stack(Push(Constant, 2)), Stack(Arithmetic(Or))];
        compile_less: Expression::op('<', 1, 2) => [Stack(Push(Constant, 1)), Stack(Push(Constant, 2)), Stack(Arithmetic(Lt))];
        compile_more: Expression::op('>', 1, 2) => [Stack(Push(Constant, 1)), Stack(Push(Constant, 2)), Stack(Arithmetic(Gt))];
        compile_equal: Expression::op('=', 0, 0) => [Stack(Push(Constant, 0)), Stack(Push(Constant, 0)), Stack(Arithmetic(Eq))];
    }

    compiler_tests! {
        compile_term:
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
        compile_exp: Term::expression(Expression::op('+', 1, 2)) =>  [Stack(Push(Constant, 1)), Stack(Push(Constant, 2)), Stack(Arithmetic(Add))];
        compile_neg: Term::neg(Term::integer(2)) => [Stack(Push(Constant, 2)), Stack(Arithmetic(Neg))];
        compile_not: Term::not(Term::False) => [Stack(Push(Constant, 0)), Stack(Arithmetic(Not))];
        compile_call_func0: Term::Call(SubroutineCall::FunctionCall("Foo.bar", vec![])) => [Call("Foo.bar", 0)];
        compile_call_func1: Term::Call(SubroutineCall::FunctionCall("Foo.bar", vec![1.into()])) => [Stack(Push(Constant, 1)), Call("Foo.bar", 1)];
        compile_call_func2: Term::Call(SubroutineCall::FunctionCall("Foo.bar", vec![1.into(), 2.into()])) => [Stack(Push(Constant, 1)), Stack(Push(Constant, 2)), Call("Foo.bar", 2)];
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

    #[test]
    fn compile_method_call0() {
        let mut compiler = Compiler::new();
        compiler.define_local("foo", Type::Class("Foo"));
        compiler
            .compile_term(Term::Call(SubroutineCall::MethodCall("foo", "bar", vec![])))
            .unwrap();
        assert_eq!(compiler.code(), [Stack(Push(Local, 0)), Call("Foo.bar", 1)]);
    }

    #[test]
    fn compile_method_call2() {
        let mut compiler = Compiler::new();
        compiler.define_local("foo", Type::Class("Foo"));
        compiler
            .compile_term(Term::Call(SubroutineCall::MethodCall(
                "foo",
                "bar",
                vec![1.into(), 2.into()],
            )))
            .unwrap();
        assert_eq!(
            compiler.code(),
            [
                Stack(Push(Local, 0)),
                Stack(Push(Constant, 1)),
                Stack(Push(Constant, 2)),
                Call("Foo.bar", 3)
            ]
        );
    }

    #[test]
    fn compile_array_index() {
        let mut compiler = Compiler::new();
        compiler.define_local("arr", Type::Class("Array"));
        compiler
            .compile_term(Term::ArrayIndex("arr", 42.into()))
            .unwrap();
        assert_eq!(
            compiler.code(),
            [
                Stack(Push(Local, 0)),
                Stack(Push(Constant, 42)),
                Stack(Arithmetic(Add)),
                Stack(Pop(Pointer, 1)),
                Stack(Push(That, 0))
            ]
        );
    }

    #[test]
    fn compile_let_statement() {
        let mut compiler = Compiler::new();
        compiler.define_local("foo", Type::Int);
        compiler
            .compile_statement(Statement::Let("foo", None, 42.into()))
            .unwrap();
        assert_eq!(
            compiler.code(),
            [Stack(Push(Constant, 42)), Stack(Pop(Local, 0))]
        );
    }

    #[test]
    fn compile_let_array_statement() {
        let mut compiler = Compiler::new();
        compiler.define_local("foo", Type::Int);
        compiler
            .compile_statement(Statement::Let("foo", Some(2.into()), 42.into()))
            .unwrap();
        assert_eq!(
            compiler.code(),
            [
                Stack(Push(Constant, 42)),
                Stack(Push(Local, 0)),
                Stack(Push(Constant, 2)),
                Stack(Arithmetic(Add)),
                Stack(Pop(Pointer, 1)),
                Stack(Pop(That, 0))
            ]
        );
    }

    #[test]
    fn compile_let_array_to_array() {
        let mut compiler = Compiler::new();
        compiler.define_local("a", Type::Int);
        compiler.define_local("b", Type::Int);
        compiler
            .compile_statement(Statement::Let(
                "a",
                Some(7.into()),
                Term::ArrayIndex("b", 9.into()).into(),
            ))
            .unwrap();
        assert_eq!(
            compiler.code(),
            [
                // push b[9]
                Stack(Push(Local, 1)),
                Stack(Push(Constant, 9)),
                Stack(Arithmetic(Add)),
                Stack(Pop(Pointer, 1)),
                Stack(Push(That, 0)),
                // pop a[7]
                Stack(Push(Local, 0)),
                Stack(Push(Constant, 7)),
                Stack(Arithmetic(Add)),
                Stack(Pop(Pointer, 1)),
                Stack(Pop(That, 0)),
            ]
        );
    }
}
