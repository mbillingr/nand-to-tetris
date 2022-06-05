use crate::chapter06_assembler::parser::Parser;
use crate::chapter07_vm::parser::{ArithmeticCmd, Command, Segment};

pub struct CodeGenerator {
    label_counter: usize,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator { label_counter: 0 }
    }

    pub fn translate(&mut self, src: &str) -> Result<String, String> {
        let mut parser = Parser::new(src);

        let mut asm_code = String::new();
        while parser.has_more_lines() {
            parser.advance();

            if let Some(cmd) = parser.instruction_str() {
                asm_code += "// ";
                asm_code += cmd;
                asm_code += "\n";
            }

            match parser.instruction()? {
                Command::Arithmetic(ac) => asm_code += &self.gen_arithmetic_cmd(ac),
                Command::Push(segment, index) => asm_code += &self.gen_push_cmd(segment, index),
                Command::Pop(_, _) => todo!(),
            }

            asm_code += "\n";
        }

        Ok(asm_code)
    }

    fn gen_push_cmd(&self, segment: Segment, index: usize) -> String {
        let mut asm = String::new();
        match segment {
            Segment::Constant => asm += &self.gen_constant(index),
            _ => {
                asm += self.gen_segment(segment);
                asm += &self.gen_offset(index);
                asm += self.gen_load();
            }
        }
        asm += self.gen_push();
        asm
    }

    fn gen_arithmetic_cmd(&mut self, cmd: ArithmeticCmd) -> String {
        match cmd {
            ArithmeticCmd::Add => self.gen_binary_cmd("D=D+A"),
            ArithmeticCmd::Sub => self.gen_binary_cmd("D=D-A"),
            ArithmeticCmd::Neg => self.gen_unary_cmd("D=-D"),
            ArithmeticCmd::Eq => self.gen_comparison_cmd("JEQ"),
            ArithmeticCmd::Gt => self.gen_comparison_cmd("JGT"),
            ArithmeticCmd::Lt => self.gen_comparison_cmd("JLT"),
            ArithmeticCmd::And => self.gen_binary_cmd("D=D&A"),
            ArithmeticCmd::Or => self.gen_binary_cmd("D=D|A"),
            ArithmeticCmd::Not => self.gen_unary_cmd("D=!D"),
        }
    }

    fn gen_unary_cmd(&self, cmd: &str) -> String {
        let mut asm = String::new();
        asm += self.gen_pop();
        asm += cmd;
        asm += "\n";
        asm += self.gen_push();
        asm
    }

    fn gen_binary_cmd(&self, cmd: &str) -> String {
        let mut asm = String::new();
        asm += self.gen_pop();
        asm += "@R15\nM=D\n";
        asm += self.gen_pop();
        asm += "@R15\nA=M\n";
        asm += cmd;
        asm += "\n";
        asm += self.gen_push();
        asm
    }

    fn gen_comparison_cmd(&mut self, cmd: &str) -> String {
        let label = self.label_counter;
        self.label_counter += 1;

        let mut asm = String::new();
        asm += self.gen_pop();
        asm += "@R15\nM=D\n";
        asm += self.gen_pop();
        asm += "@R15\nD=D-M\n";
        asm += &format!("@CMP-TRUE.{}\n", label);
        asm += "D;";
        asm += cmd;
        asm += "\n";
        asm += &format!("@CMP-END.{}\n", label);
        asm += "D=0;JMP\n";
        asm += &format!("(CMP-TRUE.{})\n", label);
        asm += "D=-1\n";
        asm += &format!("(CMP-END.{})\n", label);
        asm += self.gen_push();
        asm
    }

    fn gen_segment(&self, segment: Segment) -> &'static str {
        match segment {
            Segment::Argument => "@ARG\nA=M\n",
            _ => panic!("{:?}", segment),
        }
    }

    /// add offset to current address
    fn gen_offset(&self, index: usize) -> String {
        match index {
            0 => String::new(),
            1 => "A=A+1\n".to_string(),
            2 => "A=A+1\nA=A+1\n".to_string(),
            _ => format!("D=A\n@{}\nA=D+A\n", index),
        }
    }

    /// load constant into D register
    fn gen_constant(&self, value: usize) -> String {
        match value {
            0 => "D=0\n".to_string(),
            1 => "D=1\n".to_string(),
            _ => format!("@{}\nD=A\n", value),
        }
    }

    /// load memory into D register
    fn gen_load(&self) -> &'static str {
        "D=M\n"
    }

    /// push D register on stack
    fn gen_push(&self) -> &'static str {
        "@SP
A=M
M=D
@SP
M=M+1
"
    }

    /// pop from stack into D register
    fn gen_pop(&self) -> &'static str {
        "@SP
M=M-1
@SP
A=M
D=M
"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push() {
        assert_eq!(
            CodeGenerator::new().gen_push(),
            "@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        )
    }

    #[test]
    fn pop() {
        assert_eq!(
            CodeGenerator::new().gen_pop(),
            "@SP\nM=M-1\n@SP\nA=M\nD=M\n"
        )
    }

    #[test]
    fn load() {
        assert_eq!(CodeGenerator::new().gen_load(), "D=M\n")
    }

    #[test]
    fn offset() {
        assert_eq!(CodeGenerator::new().gen_offset(0), "");
        assert_eq!(CodeGenerator::new().gen_offset(1), "A=A+1\n");
        assert_eq!(CodeGenerator::new().gen_offset(2), "A=A+1\nA=A+1\n");
        assert_eq!(CodeGenerator::new().gen_offset(3), "D=A\n@3\nA=D+A\n");
        assert_eq!(CodeGenerator::new().gen_offset(99), "D=A\n@99\nA=D+A\n");
    }

    #[test]
    fn segment() {
        assert_eq!(
            CodeGenerator::new().gen_segment(Segment::Argument),
            "@ARG\nA=M\n"
        );
    }

    #[test]
    fn constant() {
        assert_eq!(CodeGenerator::new().gen_constant(0), "D=0\n");
        assert_eq!(CodeGenerator::new().gen_constant(1), "D=1\n");
        assert_eq!(CodeGenerator::new().gen_constant(99), "@99\nD=A\n");
    }

    #[test]
    fn unary_cmd() {
        let cg = CodeGenerator::new();
        assert_eq!(
            cg.gen_unary_cmd("cmd"),
            format!("{}cmd\n{}", cg.gen_pop(), cg.gen_push())
        );
    }

    #[test]
    fn binary_cmd() {
        let cg = CodeGenerator::new();
        assert_eq!(
            cg.gen_binary_cmd("cmd"),
            format!(
                "{}@R15\nM=D\n{}@R15\nA=M\ncmd\n{}",
                cg.gen_pop(),
                cg.gen_pop(),
                cg.gen_push()
            )
        );
    }

    #[test]
    fn comparison_cmd() {
        let mut cg = CodeGenerator::new();
        assert_eq!(cg.gen_comparison_cmd("***"), format!("{}@R15\nM=D\n{}@R15\nD=D-M\n@CMP-TRUE.0\nD;***\n@CMP-END.0\nD=0;JMP\n(CMP-TRUE.0)\nD=-1\n(CMP-END.0)\n{}", cg.gen_pop(), cg.gen_pop(), cg.gen_push()));
    }
}
