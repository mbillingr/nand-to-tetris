pub mod parser;
pub mod translator;

pub const SP: u16 = 0;
pub const STACK_START_ADDR: u16 = 256;

pub const TRUE: u16 = (-1i16) as u16;
pub const FALSE: u16 = 0;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chapter06_assembler::assembler::assemble;
    use crate::chapter07_vm::translator::CodeGenerator;
    use crate::computer_emulator::Computer;

    struct VmRunner {
        emu: Computer,
    }

    impl VmRunner {
        fn new() -> Self {
            VmRunner {
                emu: Computer::new(vec![]),
            }
        }

        fn run(&mut self, vm_code: &str) -> Result<(), String> {
            let asm_code = CodeGenerator::new().translate(&vm_code)?;
            let binary_code = assemble(&asm_code)?;

            let mut emu = Computer::new(binary_code);

            // initialize stack pointer
            emu.ram[SP as usize] = STACK_START_ADDR;

            emu.run();

            self.emu = emu;

            Ok(())
        }

        fn get_stack(&self) -> &[u16] {
            let sp = self.emu.get_ram(0) as usize;
            &self.emu.ram[256..sp]
        }
    }

    #[test]
    fn simple_add() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 1
            push constant 2
            add",
        )
        .unwrap();
        assert_eq!(vm.get_stack(), [1 + 2]);
    }

    #[test]
    fn stack_test() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 19
            push constant 19
            eq
            push constant 20
            push constant 19
            eq
            push constant 19
            push constant 20
            eq
            push constant 953
            push constant 952
            lt
            push constant 952
            push constant 953
            lt
            push constant 952
            push constant 952
            lt
            push constant 32765
            push constant 32764
            gt
            push constant 32764
            push constant 32765
            gt
            push constant 32765
            push constant 32765
            gt
            push constant 57
            push constant 31
            push constant 53
            add
            push constant 112
            sub
            neg
            and
            push constant 82
            or
            not",
        )
        .unwrap();
        assert_eq!(
            vm.get_stack(),
            [
                TRUE,
                FALSE,
                FALSE,
                FALSE,
                TRUE,
                FALSE,
                TRUE,
                FALSE,
                FALSE,
                (-91_i16) as u16
            ]
        );
    }
}
