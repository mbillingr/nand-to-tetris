pub mod parser;
pub mod translator;

pub const SP: u16 = 0;
pub const STACK_START_ADDR: u16 = 256;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chapter06_assembler::assembler::assemble;
    use crate::chapter07_vm::translator::translate;
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
            let asm_code = translate(&vm_code)?;
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
        todo!()
    }
}
