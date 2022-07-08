use crate::asm;
use crate::chapter06_assembler::parser::Parser;
use crate::chapter07_vm::optimizer::optimize_pair;
use crate::chapter07_vm::translator::{CodeGenerator as CodeGen07, POPD};
use crate::chapter08_vm::parser::Command;

pub struct CodeGenerator {
    ch7_gen: CodeGen07,
    current_function: String,
}

impl CodeGenerator {
    pub fn new(module: impl Into<String>) -> Self {
        let ch7_gen = CodeGen07::new(module);
        CodeGenerator {
            current_function: ch7_gen.get_module_name().to_string() + ".GLOBAL",
            ch7_gen,
        }
    }

    pub fn set_module_name(&mut self, module: impl Into<String>) {
        self.ch7_gen.set_module_name(module)
    }

    pub fn parse<'s>(src: &'s str) -> impl Iterator<Item = Result<Command<'s>, String>> + 's {
        let mut parser = Parser::new(src);

        (0..)
            .map(move |_| {
                if parser.has_more_lines() {
                    parser.advance();
                    Some(parser.instruction())
                } else {
                    None
                }
            })
            .take_while(Option::is_some)
            .map(Option::unwrap)
    }

    pub fn optimize<'s>(
        cmds: impl Iterator<Item = Result<Command<'s>, String>>,
    ) -> Box<dyn Iterator<Item = Result<Command<'s>, String>> + 's> {
        use Command::*;
        let mut command_buffer: Vec<Command> = vec![];
        for cmd in cmds {
            match cmd {
                Err(msg) => return Box::new([Err(msg)].into_iter()),
                Ok(cmd) => command_buffer.push(cmd),
            }

            loop {
                let b = command_buffer.pop();
                let a = command_buffer.pop();

                match (a, b) {
                    (None, None) => {}
                    (None, Some(b)) => {
                        command_buffer.push(b);
                    }
                    (Some(Stack(a)), Some(Stack(b))) => match optimize_pair(a, b) {
                        Ok(opt) => {
                            command_buffer.push(Stack(opt));
                            continue;
                        }
                        Err((a, b)) => {
                            command_buffer.push(Stack(a));
                            command_buffer.push(Stack(b));
                        }
                    },
                    (Some(a), Some(b)) => {
                        command_buffer.push(a);
                        command_buffer.push(b);
                    }
                    _ => unreachable!(),
                }

                break;
            }
        }
        Box::new(command_buffer.into_iter().map(|cmd| Ok(cmd)))
    }

    pub fn translate<'s>(
        &mut self,
        instructions: impl Iterator<Item = Result<Command<'s>, String>>,
    ) -> Result<String, String> {
        let mut asm_code = String::new();
        for instruction in instructions {
            let instruction = instruction?;
            asm_code += &format!("// {}\n", instruction);

            asm_code += &self.gen_instruction(instruction);

            asm_code += "\n";
        }

        Ok(asm_code)
    }

    pub fn gen_bootstrap(&mut self) -> String {
        let mut asm = "// bootstrap code\n".to_string();
        asm += asm!((SP = 256));
        //asm += "@Sys.init\n0;JMP";
        asm += &self.gen_funcall("Sys.init", 0);
        asm
    }

    pub fn gen_instruction(&mut self, instruction: Command) -> String {
        match instruction {
            Command::Stack(sc) => self.ch7_gen.gen_instruction(sc),
            Command::Label(label) => format!("({}${})\n", self.current_function, label),
            Command::Goto(label) => format!("@{}${}\n0;JMP\n", self.current_function, label),
            Command::IfGoto(label) => {
                format!("{POPD}@{}${}\nD;JNE\n", self.current_function, label)
            }
            Command::Function(name, n_locals) => self.gen_function(name, n_locals),
            Command::Call(name, n_args) => self.gen_funcall(name, n_args),
            Command::Return => self.gen_return(),
            Command::Halt => format!("@32767\n0;JMP\n"),
        }
    }

    fn gen_function(&mut self, name: &str, n_locals: u16) -> String {
        self.current_function = name.to_string();
        let mut asm = format!("({})\n", name);
        for _ in 0..n_locals {
            asm += &self.ch7_gen.gen_push("0");
        }
        asm
    }

    fn gen_funcall(&mut self, name: &str, n_args: u16) -> String {
        let return_label = self
            .ch7_gen
            .unique_label(format!("{}$ret", self.current_function));
        let mut asm = String::new();
        asm += &format!("@{}\nD=A\n", return_label);
        asm += &self.ch7_gen.gen_push("D");
        asm += asm!(
            ((*SP) = LCL);
            (SP = (SP + 1));
            ((*SP) = ARG);
            (SP = (SP + 1));
            ((*SP) = THIS);
            (SP = (SP + 1));
            ((*SP) = THAT);
            (SP = (SP + 1));
        );
        asm += &format!(
            "// ARG = SP - 5 - n_args\n@{}\nD=A\n@SP\nA=M\nD=A-D\n@ARG\nM=D\n",
            5 + n_args
        );
        asm += asm!((LCL = SP));
        asm += &format!("// goto f\n@{}\n0;JMP\n", name);
        asm += &format!("({})", return_label);
        asm
    }

    fn gen_return(&mut self) -> String {
        let mut asm = String::new();
        asm += asm!(
            (R15 = (*(LCL - 5)));
            ((*ARG) = (*(SP - 1)));
            (SP = (ARG + 1));
            (THAT = (*(--LCL)));
            (THIS = (*(--LCL)));
            (ARG = (*(--LCL)));
            (LCL = (*(--LCL)));
            (goto R15)
        );
        asm
    }
}
