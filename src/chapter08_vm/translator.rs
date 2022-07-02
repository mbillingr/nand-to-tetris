use crate::asm;
use crate::chapter06_assembler::parser::Parser;
use crate::chapter07_vm::optimizer::optimize_pair;
use crate::chapter07_vm::translator::{CodeGenerator as CodeGen07, POPD};
use crate::chapter08_vm::parser::Command;

pub struct CodeGenerator {
    ch7_gen: CodeGen07,
}

impl CodeGenerator {
    pub fn new(module: impl Into<String>) -> Self {
        CodeGenerator {
            ch7_gen: CodeGen07::new(module),
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

    pub fn gen_instruction(&mut self, instruction: Command) -> String {
        match instruction {
            Command::Stack(sc) => self.ch7_gen.gen_instruction(sc),
            Command::Label(label) => format!("({})\n", label),
            Command::Goto(label) => format!("@{}\n0;JMP\n", label),
            Command::IfGoto(label) => format!("{POPD}@{}\nD;JNE\n", label),
            Command::Function(name, n_locals) => self.gen_function(name, n_locals),
            Command::Return => self.gen_return(),
            _ => todo!("{:?}", instruction),
        }
    }

    fn gen_function(&mut self, name: &str, n_locals: u16) -> String {
        let mut asm = format!("({})\n", name);
        for _ in 0..n_locals {
            asm += &self.ch7_gen.gen_push("0");
        }
        asm
    }

    fn gen_return(&mut self) -> String {
        let mut asm = String::new();
        asm += asm!(
            ((*ARG) = (*(SP - 1)));
            (SP = (ARG + 1));
            (THAT = (*(--LCL)));
            (THIS = (*(--LCL)));
            (ARG = (*(--LCL)));
            (R15 = (*(LCL - 2)));
            (LCL = (*(--LCL)));
            (goto R15)
        );
        asm
    }
}
