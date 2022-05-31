use crate::chapter06_assembler::parser::{Instruction, Parser};

const C_INSTRUCTION: u16 = 0b1110000000000000;
const JUMP_OFFSET: u8 = 0;
const DEST_OFFSET: u8 = 3;
const COMP_OFFSET: u8 = 6;

pub fn assemble(src: &str) -> Result<Vec<u16>, String> {
    let mut parser = Parser::new(src);

    let mut binary_output = vec![];

    while parser.has_more_lines() {
        parser.advance();
        match parser.instruction()? {
            Instruction::C(dest, comp, jump) => binary_output.push(
                C_INSTRUCTION
                    | (dest.code() << DEST_OFFSET)
                    | (comp.code() << COMP_OFFSET)
                    | (jump.code() << JUMP_OFFSET),
            ),
            Instruction::A(s) => {
                let value = s.parse().unwrap();
                binary_output.push(value);
            }
            Instruction::L(s) => {}
        }
    }

    Ok(binary_output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assemble_empty_source() {
        assert!(assemble("").unwrap().is_empty());
    }

    #[test]
    fn assemble_c_instruction() {
        assert_eq!(assemble("D&A").unwrap(), vec![0b1110000000000000]);
        assert_eq!(assemble("ADM=1;JMP").unwrap(), vec![0b1110111111111111]);
    }

    #[test]
    fn assemble_a_instruction() {
        assert_eq!(assemble("@0").unwrap(), vec![0b0000000000000000]);
        assert_eq!(assemble("@21845").unwrap(), vec![0b0101010101010101]);
        assert_eq!(assemble("@32767").unwrap(), vec![0b0111111111111111]);
    }

    #[test]
    fn assemble_program_without_symbols() {
        assert_eq!(
            assemble("//loop-forever\n@0\n0;JMP").unwrap(),
            vec![0b0000000000000000, 0b1110101010000111]
        );
    }

    #[test]
    fn assemble_symbols() {
        assert_eq!(
            assemble("(START)\n@END\n(END)\n@START").unwrap(),
            vec![0b0000000000000001, 0b0000000000000000]
        );
    }
}
