pub mod parser;
pub mod translator;

#[cfg(test)]
mod tests {
    use crate::chapter06_assembler::assembler::{assemble, ARG, LCL, SP, THAT, THIS};
    use crate::chapter06_assembler::optimizer::PeepholeOptimizer;
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
            println!("{}", self.asm_code);
            let optimized_asm = PeepholeOptimizer::default()
                .optimize_str(&self.asm_code)
                .unwrap();
            println!("{}", optimized_asm);
            let binary_code = assemble(&optimized_asm).unwrap();

            let mut emu = Computer::new(binary_code);

            // initialize virtual register defaults
            emu.ram[SP as usize] = STACK_START_ADDR;
            emu.ram[LCL as usize] = LOCAL_START_ADDR;
            emu.ram[ARG as usize] = ARG_START_ADDR;
            emu.ram[THIS as usize] = THIS_START_ADDR;
            emu.ram[THAT as usize] = THAT_START_ADDR;

            VmRunner { emu }
        }

        fn bootstrap(&mut self) {
            self.asm_code = self.code_gen.gen_bootstrap();
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

        fn push(&mut self, value: i16) {
            let sp = self.emu.get_ram(SP) as usize;
            self.emu.ram[sp] = value as u16;
            self.emu.set_ram(SP, sp as u16 + 1)
        }

        fn get_sp_ptr(&self) -> u16 {
            self.emu.get_ram(SP)
        }

        fn set_sp_ptr(&mut self, value: u16) {
            self.emu.set_ram(SP, value)
        }

        fn get_locals(&self) -> &[u16] {
            let lcl = self.emu.get_ram(LCL) as usize;
            &self.emu.ram[lcl as usize..]
        }

        fn set_local(&mut self, offset: u16, value: u16) {
            let lcl = self.emu.get_ram(LCL);
            self.emu.set_ram(lcl + offset, value)
        }

        fn get_lcl_ptr(&self) -> u16 {
            self.emu.get_ram(LCL)
        }

        fn set_lcl_ptr(&mut self, value: u16) {
            self.emu.set_ram(LCL, value)
        }

        fn get_arg_ptr(&self) -> u16 {
            self.emu.get_ram(ARG)
        }

        fn set_arg_ptr(&mut self, value: u16) {
            self.emu.set_ram(ARG, value)
        }

        fn get_arguments(&self) -> &[u16] {
            &self.emu.ram[ARG_START_ADDR as usize..]
        }

        fn set_argument(&mut self, idx: u16, value: u16) {
            let arg = self.emu.get_ram(ARG);
            self.emu.set_ram(arg + idx, value)
        }

        fn get_this(&self) -> &[u16] {
            let this = self.emu.get_ram(THIS) as usize;
            &self.emu.ram[this..]
        }

        fn get_that(&self) -> &[u16] {
            let that = self.emu.get_ram(THAT) as usize;
            &self.emu.ram[that..]
        }

        fn get_this_ptr(&self) -> u16 {
            self.emu.get_ram(THIS)
        }

        fn set_this_ptr(&mut self, value: u16) {
            self.emu.set_ram(THIS, value)
        }

        fn get_that_ptr(&self) -> u16 {
            self.emu.get_ram(THAT)
        }

        fn set_that_ptr(&mut self, value: u16) {
            self.emu.set_ram(THAT, value)
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
        vm.set_argument(0, 4);
        vm.run();
        assert_eq!(vm.get_stack(), [10]);
    }

    #[test]
    fn fibonacci_series() {
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
        vm.set_argument(0, 8);
        vm.set_argument(1, 3000);
        vm.run();
        assert_eq!(vm.get_ram()[3000..3008], [0, 1, 1, 2, 3, 5, 8, 13]);
        assert_eq!(vm.get_stack(), []);
    }

    #[test]
    fn return_command() {
        let mut vmb = VmBuilder::new();
        vmb.add_module("JustReturn", "return").unwrap();
        let mut vm = vmb.build();
        // set up an artifical stack frame to return from
        // some dummy values
        vm.push(3);
        vm.push(2);
        vm.push(1);
        // arguments
        vm.set_arg_ptr(vm.get_sp_ptr());
        vm.push(10);
        vm.push(11);
        vm.push(12);
        // saved stats
        vm.push(-1); // return address
        vm.push(1111); // LCL
        vm.push(2222); // ARG
        vm.push(3333); // THIS
        vm.push(4444); // THAT
                       // locals
        vm.set_lcl_ptr(vm.get_sp_ptr());
        vm.push(100);
        vm.push(101);
        vm.push(102);
        println!("{:?}", vm.get_stack());
        vm.run();
        println!("{:?}", vm.get_stack());
        println!("{:?}", &vm.get_ram()[..32]);

        assert_eq!(vm.get_stack(), &[3, 2, 1, 102]);
        assert_eq!(vm.get_that_ptr(), 4444);
        assert_eq!(vm.get_this_ptr(), 3333);
        assert_eq!(vm.get_arg_ptr(), 2222);
        assert_eq!(vm.get_lcl_ptr(), 1111);
        assert_eq!(vm.emu.get_pc(), (-1_i16) as u16);
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
        vm.set_sp_ptr(317);
        vm.set_lcl_ptr(317);
        vm.set_arg_ptr(310);
        vm.set_this_ptr(3000);
        vm.set_that_ptr(4000);
        vm.set_argument(0, 1234);
        vm.set_argument(1, 37);
        vm.set_argument(2, 1000);
        vm.set_argument(3, 305);
        vm.set_argument(4, 300);
        vm.set_argument(5, 3010);
        vm.set_argument(6, 4010);
        vm.run();
        assert_eq!(vm.get_sp_ptr(), 311);
        assert_eq!(vm.get_lcl_ptr(), 305);
        assert_eq!(vm.get_arg_ptr(), 300);
        assert_eq!(vm.get_this_ptr(), 3010);
        assert_eq!(vm.get_that_ptr(), 4010);
        assert_eq!(vm.emu.get_ram(310), 1196);
        assert_eq!(vm.get_stack().last(), Some(&1196));
    }

    #[test]
    fn fibonacci_element() {
        let sys = "\
            function Sys.init 0
            push constant 9
            call Main.fibonacci 1
            label WHILE
            halt
            goto WHILE";
        let main = "\
            function Main.fibonacci 0
            push argument 0
            push constant 2
            lt
            if-goto IF_TRUE
            goto IF_FALSE
            label IF_TRUE
            push argument 0
            return
            label IF_FALSE
            push argument 0
            push constant 2
            sub
            call Main.fibonacci 1
            push argument 0
            push constant 1
            sub
            call Main.fibonacci 1
            add
            return";
        let mut vmb = VmBuilder::new();
        vmb.bootstrap();
        vmb.add_module("Main", main).unwrap();
        vmb.add_module("Sys", sys).unwrap(); // add sys last to check bootstrap code
        let mut vm = vmb.build();
        vm.run();
        assert_eq!(vm.get_stack().last(), Some(&34));
        assert_eq!(vm.get_sp_ptr(), 262);
    }
}
