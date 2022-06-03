use crate::chapter06_assembler::parser::Parser;
use crate::chapter07_vm::parser::{ArithmeticCmd, Command, Segment};

pub fn translate(src: &str) -> Result<String, String> {
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
            Command::Arithmetic(ac) => asm_code += &gen_arithmetic_cmd(ac),
            Command::Push(segment, index) => asm_code += &gen_push_cmd(segment, index),
            Command::Pop(_, _) => todo!(),
        }

        asm_code += "\n";
    }

    Ok(asm_code)
}

fn gen_push_cmd(segment: Segment, index: usize) -> String {
    let mut asm = String::new();
    match segment {
        Segment::Constant => asm += &gen_constant(index),
        _ => {
            asm += gen_segment(segment);
            asm += &gen_offset(index);
            asm += gen_load();
        }
    }
    asm += gen_push();
    asm
}

fn gen_arithmetic_cmd(cmd: ArithmeticCmd) -> String {
    let mut asm = String::new();
    asm += gen_pop();
    asm += "@R15\nM=D\n";
    asm += gen_pop();
    asm += "@R15\nA=M\n";
    match cmd {
        ArithmeticCmd::Add => asm += "D=D+A\n",
        _ => todo!(),
    }
    asm += gen_push();
    asm
}

fn gen_segment(segment: Segment) -> &'static str {
    match segment {
        Segment::Argument => "@ARG\nA=M\n",
        _ => panic!("{:?}", segment),
    }
}

/// add offset to current address
fn gen_offset(index: usize) -> String {
    match index {
        0 => String::new(),
        1 => "A=A+1\n".to_string(),
        2 => "A=A+1\nA=A+1\n".to_string(),
        _ => format!("D=A\n@{}\nA=D+A\n", index),
    }
}

/// load constant into D register
fn gen_constant(value: usize) -> String {
    match value {
        0 => "D=0\n".to_string(),
        1 => "D=1\n".to_string(),
        _ => format!("@{}\nD=A\n", value),
    }
}

/// load memory into D register
fn gen_load() -> &'static str {
    "D=M\n"
}

/// push D register on stack
fn gen_push() -> &'static str {
    "@SP
A=M
M=D
@SP
M=M+1
"
}

/// pop from stack into D register
fn gen_pop() -> &'static str {
    "@SP
M=M-1
@SP
A=M
D=M
"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push() {
        assert_eq!(gen_push(), "@SP\nA=M\nM=D\n@SP\nM=M+1\n")
    }

    #[test]
    fn pop() {
        assert_eq!(gen_pop(), "@SP\nM=M-1\n@SP\nA=M\nD=M\n")
    }

    #[test]
    fn load() {
        assert_eq!(gen_load(), "D=M\n")
    }

    #[test]
    fn offset() {
        assert_eq!(gen_offset(0), "");
        assert_eq!(gen_offset(1), "A=A+1\n");
        assert_eq!(gen_offset(2), "A=A+1\nA=A+1\n");
        assert_eq!(gen_offset(3), "D=A\n@3\nA=D+A\n");
        assert_eq!(gen_offset(99), "D=A\n@99\nA=D+A\n");
    }

    #[test]
    fn segment() {
        assert_eq!(gen_segment(Segment::Argument), "@ARG\nA=M\n");
    }

    #[test]
    fn constant() {
        assert_eq!(gen_constant(0), "D=0\n");
        assert_eq!(gen_constant(1), "D=1\n");
        assert_eq!(gen_constant(99), "@99\nD=A\n");
    }
}
