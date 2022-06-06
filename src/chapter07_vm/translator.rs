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
                Command::Pop(segment, index) => asm_code += &self.gen_pop_cmd(segment, index),
            }

            asm_code += "\n";
        }

        Ok(asm_code)
    }

    fn gen_push_cmd(&self, segment: Segment, index: u16) -> String {
        match segment {
            Segment::Constant => self.gen_push_constant(index),
            Segment::Argument => self.gen_push_offsetptr("@ARG", index),
            Segment::Local => self.gen_push_offsetptr("@LCL", index),
            Segment::This => self.gen_push_offsetptr("@THIS", index),
            Segment::That => self.gen_push_offsetptr("@THAT", index),
            Segment::Temp => self.gen_push_addr(5 + index),
            _ => todo!("{:?}", segment),
        }
    }

    fn gen_pop_cmd(&self, segment: Segment, index: u16) -> String {
        match segment {
            Segment::Constant => return String::new(), // just ignore popping into constants
            Segment::Argument => return self.gen_pop_offsetptr("@ARG", index),
            Segment::Local => return self.gen_pop_offsetptr("@LCL", index),
            Segment::This => return self.gen_pop_offsetptr("@THIS", index),
            Segment::That => return self.gen_pop_offsetptr("@THAT", index),
            Segment::Temp => return self.gen_pop_addr(5 + index),
            _ => todo!("{:?}", segment),
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
        asm += "@SP\nM=M-1\nA=M\nD=M\n";
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
            _ => format!("@{}\nD=A\n{}", value, self.gen_push("D")),
        }
    }

    fn gen_push_offsetptr(&self, ptr: &str, value: u16) -> String {
        match value {
            0 => format!("{}\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n", ptr),
            1 => format!("{}\nA=M+1\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n", ptr),
            2 => format!("{}\nA=M+1\nA=A+1\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n", ptr),
            _ => format!(
                "@{}\nD=A\n{}\nA=D+M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n",
                value, ptr
            ),
        }
    }

    fn gen_pop_offsetptr(&self, ptr: &str, value: u16) -> String {
        match value {
            0 => format!("@SP\nM=M-1\nA=M\nD=M\n{}\nA=M\nM=D", ptr),
            1 => format!("@SP\nM=M-1\nA=M\nD=M\n{}\nA=M+1\nM=D", ptr),
            2 => format!("@SP\nM=M-1\nA=M\nD=M\n{}\nA=M+1\nA=A+1\nM=D", ptr),
            3 => format!("@SP\nM=M-1\nA=M\nD=M\n{}\nA=M+1\nA=A+1\nA=A+1\nM=D", ptr),
            _ => format!(
                "@{}\nD=A\n{}\nD=D+M\n@R15\nM=D\n@SP\nM=M-1\nA=M\nD=M\n@R15\nA=M\nM=D",
                value, ptr
            ),
        }
    }

    fn gen_push_addr(&self, value: u16) -> String {
        match value {
            _ => format!("@{}\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n", value),
        }
    }

    fn gen_pop_addr(&self, value: u16) -> String {
        match value {
            _ => format!("@SP\nM=M-1\nA=M\nD=M\n@{}\nM=D", value),
        }
    }

    /// push D register on stack
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push() {
        assert_eq!(
            CodeGenerator::new().gen_push("D"),
            "@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        )
    }

    #[test]
    fn unary_cmd() {
        let cg = CodeGenerator::new();
        assert_eq!(cg.gen_unary_cmd("cmd"), "@SP\nA=M-1\ncmd\n");
    }

    #[test]
    fn binary_cmd() {
        let cg = CodeGenerator::new();
        assert_eq!(
            cg.gen_binary_cmd("cmd"),
            "@SP\nM=M-1\nA=M\nD=M\n@SP\nA=M-1\ncmd\n"
        );
    }

    #[test]
    fn comparison_cmd() {
        let mut cg = CodeGenerator::new();
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
            CodeGenerator::new().gen_push_constant(2),
            "@2\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new().gen_push_constant(0),
            "@SP\nA=M\nM=0\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new().gen_push_constant(1),
            "@SP\nA=M\nM=1\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new().gen_push_constant((-1_i16) as u16),
            "@SP\nA=M\nM=-1\n@SP\nM=M+1\n"
        );
    }

    #[test]
    fn push_ptr() {
        assert_eq!(
            CodeGenerator::new().gen_push_offsetptr("@ThePtr", 3),
            "@3\nD=A\n@ThePtr\nA=D+M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new().gen_push_offsetptr("@ThePtr", 0),
            "@ThePtr\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new().gen_push_offsetptr("@ThePtr", 1),
            "@ThePtr\nA=M+1\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
        assert_eq!(
            CodeGenerator::new().gen_push_offsetptr("@ThePtr", 2),
            "@ThePtr\nA=M+1\nA=A+1\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
    }

    #[test]
    fn pop_ptr() {
        assert_eq!(
            CodeGenerator::new().gen_pop_offsetptr("@ThePtr", 99),
            "@99\nD=A\n@ThePtr\nD=D+M\n@R15\nM=D\n\
            @SP\nM=M-1\nA=M\nD=M\n\
            @R15\nA=M\nM=D"
        );
        assert_eq!(
            CodeGenerator::new().gen_pop_offsetptr("@ThePtr", 0),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @ThePtr\nA=M\nM=D"
        );
        assert_eq!(
            CodeGenerator::new().gen_pop_offsetptr("@ThePtr", 1),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @ThePtr\nA=M+1\nM=D"
        );
        assert_eq!(
            CodeGenerator::new().gen_pop_offsetptr("@ThePtr", 2),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @ThePtr\nA=M+1\nA=A+1\nM=D"
        );
        assert_eq!(
            CodeGenerator::new().gen_pop_offsetptr("@ThePtr", 3),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @ThePtr\nA=M+1\nA=A+1\nA=A+1\nM=D"
        );
    }

    #[test]
    fn push_addr() {
        assert_eq!(
            CodeGenerator::new().gen_push_addr(99),
            "@99\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
        );
    }

    #[test]
    fn pop_addr() {
        assert_eq!(
            CodeGenerator::new().gen_pop_addr(99),
            "@SP\nM=M-1\nA=M\nD=M\n\
            @99\nM=D"
        );
    }
}
