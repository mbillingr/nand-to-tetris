pub mod parser;
pub mod translator;

#[cfg(test)]
mod tests {
    use crate::chapter06_assembler::assembler::{assemble, ARG, LCL, SP, THAT, THIS};
    use crate::chapter07_vm::STACK_START_ADDR;
    use crate::chapter08_vm::translator::CodeGenerator;
    use crate::computer_emulator::Computer;

    const LOCAL_START_ADDR: u16 = 300;
    const ARG_START_ADDR: u16 = 400;
    const THIS_START_ADDR: u16 = 3000;
    const THAT_START_ADDR: u16 = 3010;
    const TEMP_START_ADDR: u16 = 5;
    const TEMP_END_ADDR: u16 = 13;

    struct VmBuilder {
        code_gen: CodeGenerator,
        asm_code: String,
    }

    impl VmBuilder {
        fn new() -> Self {
            VmBuilder {
                code_gen: CodeGenerator::new("VM"),
                asm_code: String::new(),
            }
        }

        fn build(&self) -> VmRunner {
            let binary_code = assemble(&self.asm_code).unwrap();

            let mut emu = Computer::new(binary_code);

            // initialize virtual register defaults
            emu.ram[SP as usize] = STACK_START_ADDR;
            emu.ram[LCL as usize] = LOCAL_START_ADDR;
            emu.ram[ARG as usize] = ARG_START_ADDR;
            emu.ram[THIS as usize] = THIS_START_ADDR;
            emu.ram[THAT as usize] = THAT_START_ADDR;

            VmRunner { emu }
        }

        fn add_module(&mut self, name: &str, vm_code: &str) -> Result<(), String> {
            let vm_instructions = CodeGenerator::parse(vm_code);
            let vm_instructions = CodeGenerator::optimize(vm_instructions);

            self.code_gen.set_module_name(name);

            self.asm_code += &self.code_gen.translate(vm_instructions)?;

            Ok(())
        }
    }

    struct VmRunner {
        emu: Computer,
    }

    impl VmRunner {
        fn new() -> Self {
            let mut emu = Computer::new(vec![]);

            // initialize virtual registers
            emu.ram[SP as usize] = STACK_START_ADDR;
            emu.ram[LCL as usize] = LOCAL_START_ADDR;
            emu.ram[ARG as usize] = ARG_START_ADDR;
            emu.ram[THIS as usize] = THIS_START_ADDR;
            emu.ram[THAT as usize] = THAT_START_ADDR;

            VmRunner { emu }
        }

        fn run(&mut self) {
            self.emu.run()
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

        fn set_arg(&mut self, idx: usize, value: u16) {
            self.emu.ram[idx + ARG_START_ADDR as usize] = value
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
    fn basic_loop() {
        let mut vmb = VmBuilder::new();
        vmb.add_module(
            "BasicLoop",
            "
            push constant 0
            pop local 0
            label LOOP
            push argument 0
            push local 0
            add
            pop local 0
            push argument 0
            push constant 1
            sub
            pop argument 0
            push argument 0
            if-goto LOOP
            push local 0
            ",
        )
        .unwrap();
        let mut vm = vmb.build();
        vm.set_arg(0, 4);
        vm.run();
        assert_eq!(vm.get_stack(), [10]);
    }

    #[test]
    fn fibonacci() {
        let mut vmb = VmBuilder::new();
        vmb.add_module(
            "Fibonacci",
            "
            push argument 1
            pop pointer 1

            push constant 0
            pop that 0
            push constant 1
            pop that 1

            push argument 0
            push constant 2
            sub
            pop argument 0

            label MAIN_LOOP

            push argument 0
            if-goto COMPUTE_ELEM
            goto END_PROGRAM

            label COMPUTE_ELEM
            push that 0
            push that 1
            add
            pop that 2

            push pointer 1
            push constant 1
            add
            pop pointer 1

            push argument 0
            push constant 1
            sub
            pop argument 0

            goto MAIN_LOOP

            label END_PROGRAM
            ",
        )
        .unwrap();
        let mut vm = vmb.build();
        vm.set_arg(0, 8);
        vm.set_arg(1, 3000);
        vm.run();
        assert_eq!(vm.get_ram()[3000..3008], [0, 1, 1, 2, 3, 5, 8, 13]);
    }

    #[test]
    fn simple_function() {
        let mut vmb = VmBuilder::new();
        vmb.add_module(
            "SimpleFunction",
            "
            function SimpleFunction.test 2
            push local 0
            push local 1
            add
            not
            push argument 0
            add
            push argument 1
            sub
            return
            ",
        )
        .unwrap();
        let mut vm = vmb.build();
        todo!("set up runtime");
        vm.run();
        todo!("assert results")
    }
}
