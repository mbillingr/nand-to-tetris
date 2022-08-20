#[macro_use]
pub mod hardware;
pub mod chapter01_boolean_logic;
pub mod chapter02_boolean_arithmetic;
pub mod chapter03_memory;
pub mod chapter05_computer_architecture;
pub mod chapter06_assembler;
pub mod chapter07_vm;
pub mod chapter08_vm;
pub mod chapter10_parser;
pub mod chapter11_compiler;
pub mod chapter12_ostd;
pub mod computer_emulator;
pub mod file_utils;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
