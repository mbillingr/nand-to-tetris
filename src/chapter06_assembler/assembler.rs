use crate::chapter06_assembler::parser::{Instruction, Parser};
use crate::chapter06_assembler::symbol_table::SymbolTable;

const C_INSTRUCTION: u16 = 0b1110000000000000;
const JUMP_OFFSET: u8 = 0;
const DEST_OFFSET: u8 = 3;
const COMP_OFFSET: u8 = 6;

pub fn assemble(src: &str) -> Result<Vec<u16>, String> {
    let mut symbol_table = build_symbol_table(src)?;
    let next_var_addr = predefine_variables(&mut symbol_table)?;
    generate_code(src, next_var_addr, symbol_table)
}

fn predefine_variables(symbol_table: &mut SymbolTable) -> Result<u16, String> {
    for r in 0..16 {
        symbol_table.add_entry(&format!("R{}", r), r)?;
    }
    symbol_table.add_entry("SP", 0)?;
    symbol_table.add_entry("LCL", 1)?;
    symbol_table.add_entry("ARG", 2)?;
    symbol_table.add_entry("THIS", 3)?;
    symbol_table.add_entry("THAT", 4)?;
    symbol_table.add_entry("SCREEN", 0x4000)?;
    symbol_table.add_entry("KBD", 0x6000)?;
    Ok(16)
}

fn build_symbol_table(src: &str) -> Result<SymbolTable, String> {
    let mut symbol_table = SymbolTable::new();

    let mut parser = Parser::new(src);
    let mut address = 0;
    while parser.has_more_lines() {
        parser.advance();
        match parser.instruction()? {
            Instruction::L(symbol) => symbol_table.add_entry(symbol, address)?,
            Instruction::A(_) | Instruction::C(_, _, _) => address += 1,
        }
    }

    Ok(symbol_table)
}

fn generate_code(
    src: &str,
    mut next_var_addr: u16,
    mut symbol_table: SymbolTable,
) -> Result<Vec<u16>, String> {
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
            Instruction::A(symbol) => {
                let value = match symbol
                    .parse()
                    .ok()
                    .or_else(|| symbol_table.get_address(symbol))
                {
                    Some(x) => x,
                    None => {
                        let addr = next_var_addr;
                        symbol_table.add_entry(symbol, addr)?;
                        next_var_addr += 1;
                        addr
                    }
                };
                binary_output.push(value);
            }
            Instruction::L(_) => {}
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

    #[test]
    fn assemble_variable_symbols() {
        assert_eq!(
            assemble("// x = 1\n@x\nM=1").unwrap(),
            vec![0b0000000000010000, 0b1110111111001000]
        );
    }
}
