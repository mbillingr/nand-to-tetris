use crate::chapter06_assembler::assembler::THIS;
use crate::chapter06_assembler::parser::Parser;
use crate::chapter07_vm::optimizer::optimize_pair;
use crate::chapter07_vm::parser::{ArithmeticCmd, Command, Segment};

pub struct CodeGenerator {
    module_name: String,
    label_counter: usize,
}

impl CodeGenerator {
    pub fn new(module: impl Into<String>) -> Self {
        CodeGenerator {
            module_name: module.into(),
            label_counter: 0,
        }
    }

    pub fn set_module_name(&mut self, module: impl Into<String>) {
        self.module_name = module.into()
    }

    pub fn parse<'s>(src: &'s str) -> impl Iterator<Item = Result<Command, String>> + 's {
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

    pub fn optimize(
        cmds: impl Iterator<Item = Result<Command, String>>,
    ) -> Box<dyn Iterator<Item = Result<Command, String>>> {
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
                    (None, None) => break,
                    (None, Some(b)) => {
                        command_buffer.push(b);
                        break;
                    }
                    (Some(a), Some(b)) => match optimize_pair(a, b) {
                        Ok(opt) => command_buffer.push(opt),
                        Err((a, b)) => {
                            command_buffer.push(a);
                            command_buffer.push(b);
                            break;
                        }
                    },
                    _ => unreachable!(),
                }
            }
        }
        Box::new(command_buffer.into_iter().map(|cmd| Ok(cmd)))
    }

    pub fn translate(
        &mut self,
        instructions: impl Iterator<Item = Result<Command, String>>,
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
            Command::Arithmetic(ac) => self.gen_arithmetic_cmd(ac),
            Command::Push(segment, index) => self.gen_push_cmd(segment, index),
            Command::Pop(segment, index) => self.gen_pop_cmd(segment, index),
        }
    }

    fn gen_push_cmd(&self, segment: Segment, index: u16) -> String {
        match segment {
            Segment::Constant => self.gen_push_constant(index),
            Segment::Temp => self.gen_push_addr(5 + index),
            Segment::Pointer => return self.gen_push_addr(THIS + index),
            Segment::Static => return self.gen_push_static(index),
            s => self.gen_push_offsetptr(s.register_name().unwrap(), index),
        }
    }

    fn gen_pop_cmd(&self, segment: Segment, index: u16) -> String {
        match segment {
            Segment::Constant => return String::new(), // just ignore popping into constants
            Segment::Temp => return self.gen_pop_addr(5 + index),
            Segment::Pointer => return self.gen_pop_addr(THIS + index),
            Segment::Static => return self.gen_pop_static(index),
            s => self.gen_pop_offsetptr(s.register_name().unwrap(), index),
        }
    }

    fn gen_arithmetic_cmd(&mut self, cmd: ArithmeticCmd) -> String {
        match cmd {
            ArithmeticCmd::Add => self.gen_binary_cmd("M=D+M"),
            ArithmeticCmd::Sub => self.gen_binary_cmd("M=M-D"),
            ArithmeticCmd::Neg => self.gen_unary_cmd("M=-M"),
            ArithmeticCmd::Eq => self.gen_comparison_cmd("JEQ"),
            ArithmeticCmd::Gt => self.gen_comparison_cmd("JGT"),
            ArithmeticCmd::Lt => self.gen_comparison_cmd("JLT"),
            ArithmeticCmd::And => self.gen_binary_cmd("M=D&M"),
            ArithmeticCmd::Or => self.gen_binary_cmd("M=D|M"),
            ArithmeticCmd::Not => self.gen_unary_cmd("M=!M"),
        }
    }

    /// Command with one argument. Expects input and output in M register and preserve A.
    fn gen_unary_cmd(&self, cmd: &str) -> String {
        let mut asm = String::new();
        asm += "@SP\nA=M-1\n";
        asm += cmd;
        asm += "\n";
        asm
    }

    /// Command with two arguments. Expects inputs in M and D registers, output in M, and preserve A.
    fn gen_binary_cmd(&self, cmd: &str) -> String {
        let mut asm = String::new();
        asm += POPD;
        asm += &self.gen_unary_cmd(cmd);
        asm
    }

    fn gen_comparison_cmd(&mut self, cmd: &str) -> String {
        let label = self.label_counter;
        self.label_counter += 1;

        let mut asm = String::new();
        asm += &self.gen_binary_cmd("D=M-D");
        asm += &format!("@CMP-TRUE.{}\n", label);
        asm += "D;";
        asm += cmd;
        asm += "\n";
        asm += "@SP\nA=M-1\nM=0\n";
        asm += &format!("@CMP-END.{}\n", label);
        asm += "0;JMP\n";
        asm += &format!("(CMP-TRUE.{})\n", label);
        asm += "@SP\nA=M-1\nM=-1\n";
        asm += &format!("(CMP-END.{})\n", label);
        asm
    }

    fn gen_push_constant(&self, value: u16) -> String {
        match value {
            0 => self.gen_push("0"),
            1 => self.gen_push("1"),
            65535 => self.gen_push("-1"),
            _ => format!("@{}\nD=A\n{PUSHD}", value),
        }
    }

    fn gen_push_offsetptr(&self, ptr: &str, offset: u16) -> String {
        format!("{}D=M\n{PUSHD}", self.gen_compute_offset(ptr, offset))
    }

    fn gen_pop_offsetptr(&self, ptr: &str, offset: u16) -> String {
        match offset {
            0 | 1 | 2 | 3 => format!("{POPD}{}M=D", self.gen_compute_offset(ptr, offset)),
            _ => format!(
                "@{}\nD=A\n@{}\nD=D+M\n@R15\nM=D\n{POPD}@R15\nA=M\nM=D",
                offset, ptr
            ),
        }
    }

    fn gen_compute_offset(&self, ptr: &str, offset: u16) -> String {
        match offset {
            0 => format!("@{}\nA=M\n", ptr),
            1 => format!("@{}\nA=M+1\n", ptr),
            2 => format!("@{}\nA=M+1\nA=A+1\n", ptr),
            3 => format!("@{}\nA=M+1\nA=A+1\nA=A+1\n", ptr),
            _ => format!("@{}\nD=A\n@{}\nA=D+M\n", offset, ptr),
        }
    }

    fn gen_push_addr(&self, value: u16) -> String {
        format!("@{}\nD=M\n{PUSHD}", value)
    }

    fn gen_pop_addr(&self, value: u16) -> String {
        format!("{POPD}@{}\nM=D", value)
    }

    fn gen_push_static(&self, index: u16) -> String {
        format!("@{}.{}\nD=M\n{PUSHD}", self.module_name, index)
    }

    fn gen_pop_static(&self, index: u16) -> String {
        format!("{POPD}@{}.{}\nM=D", self.module_name, index)
    }

    fn gen_push(&self, comp: &str) -> String {
        format!(
            "\
@SP
A=M
M={}
@SP
M=M+1
",
            comp
        )
    }
}

pub const POPD: &'static str = "@SP\nM=M-1\nA=M\nD=M\n";
pub const PUSHD: &'static str = "@SP\nA=M\nM=D\n@SP\nM=M+1\n";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push() {
        assert_eq!(
            CodeGenerator::new("").gen_push("D"),
            "@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        )
    }

    #[test]
    fn unary_cmd() {
        let cg = CodeGenerator::new("");
        assert_eq!(cg.gen_unary_cmd("cmd"), "@SP\nA=M-1\ncmd\n");
    }

    #[test]
    fn binary_cmd() {
        let cg = CodeGenerator::new("");
        assert_eq!(
            cg.gen_binary_cmd("cmd"),
            "@SP\nM=M-1\nA=M\nD=M\n@SP\nA=M-1\ncmd\n"
        );
    }

    #[test]
    fn comparison_cmd() {
        let mut cg = CodeGenerator::new("");
        assert_eq!(
            cg.gen_comparison_cmd("***"),
            format!(
                "\
                @SP\nM=M-1\nA=M\nD=M\n\
                @SP\nA=M-1\nD=M-D\n\
                @CMP-TRUE.0\nD;***\n\
                @SP\nA=M-1\nM=0\n\
                @CMP-END.0\n0;JMP\n\
                (CMP-TRUE.0)\n\
                @SP\nA=M-1\nM=-1\n\
                (CMP-END.0)\n"
            )
        );
    }

    #[test]
    fn push_constant() {
        assert_eq!(
            CodeGenerator::new("").gen_push_constant(2),
            "@2\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new("").gen_push_constant(0),
            "@SP\nA=M\nM=0\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new("").gen_push_constant(1),
            "@SP\nA=M\nM=1\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new("").gen_push_constant((-1_i16) as u16),
            "@SP\nA=M\nM=-1\n@SP\nM=M+1\n"
        );
    }

    #[test]
    fn push_ptr() {
        assert_eq!(
            CodeGenerator::new("").gen_push_offsetptr("ThePtr", 4),
            "@4\nD=A\n@ThePtr\nA=D+M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new("").gen_push_offsetptr("ThePtr", 0),
            "@ThePtr\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new("").gen_push_offsetptr("ThePtr", 1),
            "@ThePtr\nA=M+1\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new("").gen_push_offsetptr("ThePtr", 2),
            "@ThePtr\nA=M+1\nA=A+1\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new("").gen_push_offsetptr("ThePtr", 3),
            "@ThePtr\nA=M+1\nA=A+1\nA=A+1\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
    }

    #[test]
    fn pop_ptr() {
        assert_eq!(
            CodeGenerator::new("").gen_pop_offsetptr("ThePtr", 99),
            "@99\nD=A\n@ThePtr\nD=D+M\n@R15\nM=D\n\
            @SP\nM=M-1\nA=M\nD=M\n\
            @R15\nA=M\nM=D"
        );
        assert_eq!(
            CodeGenerator::new("").gen_pop_offsetptr("ThePtr", 0),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @ThePtr\nA=M\nM=D"
        );
        assert_eq!(
            CodeGenerator::new("").gen_pop_offsetptr("ThePtr", 1),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @ThePtr\nA=M+1\nM=D"
        );
        assert_eq!(
            CodeGenerator::new("").gen_pop_offsetptr("ThePtr", 2),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @ThePtr\nA=M+1\nA=A+1\nM=D"
        );
        assert_eq!(
            CodeGenerator::new("").gen_pop_offsetptr("ThePtr", 3),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @ThePtr\nA=M+1\nA=A+1\nA=A+1\nM=D"
        );
    }

    #[test]
    fn push_addr() {
        assert_eq!(
            CodeGenerator::new("").gen_push_addr(99),
            "@99\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
    }

    #[test]
    fn pop_addr() {
        assert_eq!(
            CodeGenerator::new("").gen_pop_addr(99),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @99\nM=D"
        );
    }

    #[test]
    fn pop_static() {
        assert_eq!(
            CodeGenerator::new("foo").gen_pop_static(99),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @foo.99\nM=D"
        );
    }
}
