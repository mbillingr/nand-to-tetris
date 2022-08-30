use nand_to_tetris::chapter06_assembler::parser::Instruction;
use nand_to_tetris::chapter07_vm::parser::{ArithmeticCmd, Command as StackCmd, Segment};
use nand_to_tetris::chapter08_vm::parser::Command;
use nand_to_tetris::chapter08_vm::translator::CodeGenerator;
use std::collections::HashMap;
/**
Read vm function definitions from stdin, then run Sys.init()
 **/
use std::io;
use std::io::Read;
use std::sync::Arc;

const FALSE: u16 = 0x0000;
const TRUE: u16 = 0xFFFF;

fn main() -> Result<(), String> {
    let mut buffer = vec![];
    io::stdin()
        .read_to_end(&mut buffer)
        .map_err(|e| e.to_string())?;

    let src = String::from_utf8(buffer).map_err(|e| e.to_string())?;

    let vm_instructions = CodeGenerator::parse(&src);

    let mut vm = Vm::new();
    vm.load_program(vm_instructions)?;

    //let iodev = Arc::new(IoDevice::new());

    println!("{}", vm.run_function("Sys.init", vec![]) as i16);

    Ok(())
}

struct Vm<'s> {
    heap: Vec<u16>,
    statics: Vec<u16>,
    stack: Vec<u16>,
    args: Vec<u16>,
    locals: Vec<u16>,
    this: u16,
    that: u16,

    call_stack: Vec<(usize, Vec<u16>, Vec<u16>, u16, u16)>,

    code: Vec<Command<'s>>,
    functions: HashMap<&'s str, (u16, usize)>,
    labels: HashMap<&'s str, usize>,
}

impl<'s> Vm<'s> {
    pub fn new() -> Self {
        Vm {
            heap: vec![0; 16384],
            statics: vec![],
            stack: vec![],
            args: vec![],
            locals: vec![],
            this: 0,
            that: 0,
            call_stack: vec![],
            code: vec![],
            functions: HashMap::new(),
            labels: HashMap::new(),
        }
    }

    pub fn load_program<T>(
        &mut self,
        prog: impl Iterator<Item = Result<Command<'s>, T>>,
    ) -> Result<(), T> {
        let mut code = vec![];
        let mut functions = HashMap::new();
        let mut labels = HashMap::new();

        for op in prog {
            match op? {
                Command::Function(name, nl) => {
                    functions.insert(name, (nl, code.len()));
                }
                Command::Label(label) => {
                    labels.insert(label, code.len());
                }
                op => code.push(op),
            }
        }

        println!("{:?}", functions);
        self.code = code;
        self.functions = functions;
        self.labels = labels;

        Ok(())
    }

    pub fn run_function(&mut self, fname: &str, args: Vec<u16>) -> u16 {
        let (mut n_locals, mut ip) = self.functions[fname];
        self.locals = vec![0; n_locals as usize];
        self.args = args;

        let mut temp = [0u16; 64];

        loop {
            let op = self.code[ip];
            //println!("{ip}: {op}");
            ip += 1;
            match op {
                Command::Stack(StackCmd::Push(Segment::Constant, val)) => self.push(val),
                Command::Stack(StackCmd::Push(Segment::Pointer, 0)) => self.push(self.this),
                Command::Stack(StackCmd::Push(Segment::Pointer, 1)) => self.push(self.that),
                Command::Stack(StackCmd::Push(Segment::This, idx)) => {
                    self.push(self.heap[(self.this + idx) as usize])
                }
                Command::Stack(StackCmd::Push(Segment::That, idx)) => {
                    self.push(self.heap[(self.that + idx) as usize])
                }
                Command::Stack(StackCmd::Push(Segment::Argument, idx)) => {
                    self.push(self.args[idx as usize])
                }
                Command::Stack(StackCmd::Push(Segment::Local, idx)) => {
                    self.push(self.locals[idx as usize])
                }
                Command::Stack(StackCmd::Push(Segment::Static, idx)) => {
                    let val = self.get_static(idx);
                    self.push(val);
                }
                Command::Stack(StackCmd::Push(Segment::Temp, idx)) => self.push(temp[idx as usize]),
                Command::Stack(StackCmd::Pop(Segment::Pointer, 0)) => self.this = self.pop(),
                Command::Stack(StackCmd::Pop(Segment::Pointer, 1)) => self.that = self.pop(),
                Command::Stack(StackCmd::Pop(Segment::This, idx)) => {
                    self.heap[(self.this + idx) as usize] = self.pop()
                }
                Command::Stack(StackCmd::Pop(Segment::That, idx)) => {
                    self.heap[(self.that + idx) as usize] = self.pop()
                }
                Command::Stack(StackCmd::Pop(Segment::Argument, idx)) => {
                    self.args[idx as usize] = self.pop()
                }
                Command::Stack(StackCmd::Pop(Segment::Local, idx)) => {
                    self.locals[idx as usize] = self.pop()
                }
                Command::Stack(StackCmd::Pop(Segment::Static, idx)) => {
                    let val = self.pop();
                    self.set_static(idx, val);
                }
                Command::Stack(StackCmd::Pop(Segment::Temp, idx)) => {
                    temp[idx as usize] = self.pop()
                }
                Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Add)) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(u16::wrapping_add(a, b))
                }
                Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Sub)) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(u16::wrapping_sub(a, b))
                }
                Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Eq)) => {
                    let b = self.pop();
                    let a = self.pop();
                    if a == b {
                        self.push(TRUE);
                    } else {
                        self.push(FALSE);
                    }
                }
                Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Gt)) => {
                    let b = self.pop();
                    let a = self.pop();
                    if a > b {
                        self.push(TRUE);
                    } else {
                        self.push(FALSE);
                    }
                }
                Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Not)) => {
                    let b = self.pop();
                    if b == 0 {
                        self.push(TRUE);
                    } else {
                        self.push(FALSE);
                    }
                }
                Command::Stack(StackCmd::Arithmetic(ArithmeticCmd::Neg)) => {
                    let x = self.pop() as i16;
                    self.push((-x) as u16);
                }
                Command::IfGoto(label) => {
                    if self.pop() != 0 {
                        ip = self.labels[label];
                    }
                }

                Command::Call(name, n_args) => match self.functions.get(name).copied() {
                    Some((n_locals, addr)) => {
                        let locals = vec![0; n_locals as usize];
                        let mut args = vec![0; n_args as usize];

                        for a in args.iter_mut().rev() {
                            *a = self.pop();
                        }

                        self.call_stack.push((
                            ip,
                            std::mem::replace(&mut self.args, args),
                            std::mem::replace(&mut self.locals, locals),
                            self.this,
                            self.that,
                        ));
                        ip = addr;
                    }
                    None => match name {
                        "Math.multiply" => {
                            let b = self.pop() as i16;
                            let a = self.pop() as i16;
                            self.push(i16::wrapping_mul(a, b) as u16)
                        }
                        "Math.divide" => {
                            let b = self.pop() as i16;
                            let a = self.pop() as i16;
                            self.push(i16::wrapping_div(a, b) as u16)
                        }
                        _ => panic!("Unknown function {name}"),
                    },
                },
                Command::Return => {
                    if self.call_stack.is_empty() {
                        return self.pop();
                    }
                    let (retip, args, locals, this, that) = self.call_stack.pop().unwrap();
                    ip = retip;
                    self.args = args;
                    self.locals = locals;
                    self.this = this;
                    self.that = that;
                }
                Command::Halt => return self.pop(),
                _ => todo!("{}", op),
            }
        }
    }

    pub fn get_static(&mut self, idx: u16) -> u16 {
        let idx = idx as usize;
        if idx >= self.statics.len() {
            self.statics.resize(self.statics.len().max(1) * 2, 0);
        }
        self.statics[idx]
    }

    pub fn set_static(&mut self, idx: u16, val: u16) {
        let idx = idx as usize;
        if idx >= self.statics.len() {
            self.statics.resize(self.statics.len().max(1) * 2, 0);
        }
        self.statics[idx] = val;
    }

    pub fn push(&mut self, val: u16) {
        self.stack.push(val);
    }

    pub fn pop(&mut self) -> u16 {
        self.stack.pop().unwrap()
    }
}
