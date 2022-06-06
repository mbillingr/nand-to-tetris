pub mod parser;
pub mod translator;

pub const STACK_START_ADDR: u16 = 256;

pub const TRUE: u16 = (-1i16) as u16;
pub const FALSE: u16 = 0;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chapter06_assembler::assembler::{assemble, ARG, LCL, SP, THAT, THIS};
    use crate::chapter07_vm::translator::CodeGenerator;
    use crate::computer_emulator::Computer;

    pub const LOCAL_START_ADDR: u16 = 300;
    pub const ARG_START_ADDR: u16 = 400;
    pub const THIS_START_ADDR: u16 = 3000;
    pub const THAT_START_ADDR: u16 = 3010;
    pub const TEMP_START_ADDR: u16 = 5;
    pub const TEMP_END_ADDR: u16 = 13;

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
            let asm_code = CodeGenerator::new("VM").translate(&vm_code)?;
            let binary_code = assemble(&asm_code)?;

            let mut emu = Computer::new(binary_code);

            // initialize virtual registers
            emu.ram[SP as usize] = STACK_START_ADDR;
            emu.ram[LCL as usize] = LOCAL_START_ADDR;
            emu.ram[ARG as usize] = ARG_START_ADDR;
            emu.ram[THIS as usize] = THIS_START_ADDR;
            emu.ram[THAT as usize] = THAT_START_ADDR;

            emu.run();

            self.emu = emu;

            Ok(())
        }

        fn get_ram(&self) -> &[u16] {
            &self.emu.ram
        }

        fn get_stack(&self) -> &[u16] {
            let sp = self.emu.get_ram(SP) as usize;
            &self.emu.ram[STACK_START_ADDR as usize..sp]
        }

        fn get_locals(&self) -> &[u16] {
            &self.emu.ram[LOCAL_START_ADDR as usize..]
        }

        fn get_args(&self) -> &[u16] {
            &self.emu.ram[ARG_START_ADDR as usize..]
        }

        fn get_this(&self) -> &[u16] {
            let this = self.emu.get_ram(THIS) as usize;
            &self.emu.ram[this..]
        }

        fn get_that(&self) -> &[u16] {
            let that = self.emu.get_ram(THAT) as usize;
            &self.emu.ram[that..]
        }

        fn get_temp(&self) -> &[u16] {
            &self.emu.ram[TEMP_START_ADDR as usize..TEMP_END_ADDR as usize]
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
    fn simple_sub() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 5
            push constant 3
            sub",
        )
        .unwrap();
        assert_eq!(vm.get_stack(), [2]);
    }

    #[test]
    fn simple_and() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 5
            push constant 6
            and",
        )
        .unwrap();
        assert_eq!(vm.get_stack(), [4]);
    }

    #[test]
    fn simple_or() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 5
            push constant 6
            or",
        )
        .unwrap();
        assert_eq!(vm.get_stack(), [7]);
    }

    #[test]
    fn simple_neg() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 5
            neg",
        )
        .unwrap();
        assert_eq!(vm.get_stack(), [(-5i16) as u16]);
    }

    #[test]
    fn simple_not() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 5
            not",
        )
        .unwrap();
        assert_eq!(vm.get_stack(), [0b1111111111111010]);
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

    #[test]
    fn basic_memory() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 10
            pop local 0
            push constant 21
            push constant 22
            pop argument 2
            pop argument 1
            push constant 36
            pop this 6
            push constant 42
            push constant 45
            pop that 5
            pop that 2
            push constant 510
            pop temp 6
            push local 0
            push that 5
            add
            push argument 1
            sub
            push this 6
            push this 6
            add
            sub
            push temp 6
            add
            ",
        )
        .unwrap();
        assert_eq!(vm.get_locals()[0], 10);
        assert_eq!(vm.get_args()[..3], [0, 21, 22]);
        assert_eq!(vm.get_this()[..7], [0, 0, 0, 0, 0, 0, 36]);
        assert_eq!(vm.get_that()[..6], [0, 0, 42, 0, 0, 45]);
        assert_eq!(vm.get_temp(), [0, 0, 0, 0, 0, 0, 510, 0]);
        assert_eq!(vm.get_stack(), [472]);
    }

    #[test]
    fn pointer() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 3030
            pop pointer 0
            push constant 3040
            pop pointer 1
            push constant 32
            pop this 2
            push constant 46
            pop that 6
            push pointer 0
            push pointer 1
            add
            push this 2
            sub
            push that 6
            add
            ",
        )
        .unwrap();
        assert_eq!(vm.get_stack(), [6084]);
        assert_eq!(vm.get_ram()[THIS as usize], 3030);
        assert_eq!(vm.get_ram()[THAT as usize], 3040);
        assert_eq!(vm.get_this()[..3], [0, 0, 32]);
        assert_eq!(vm.get_that()[..7], [0, 0, 0, 0, 0, 0, 46]);
    }

    #[test]
    fn statics() {
        let mut vm = VmRunner::new();
        vm.run(
            "
            push constant 111
            push constant 333
            push constant 888
            pop static 8
            pop static 3
            pop static 1
            push static 3
            push static 1
            sub
            push static 8
            add
            ",
        )
        .unwrap();
        assert_eq!(vm.get_stack(), [1110]);
        assert_eq!(vm.get_ram()[16..20], [888, 333, 111, 0]);
    }
}
