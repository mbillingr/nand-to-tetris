#[macro_use]
pub mod hardware;
pub mod chapter01_boolean_logic;
pub mod chapter02_boolean_arithmetic;
pub mod chapter03_memory;
pub mod chapter05_computer_architecture;
pub mod chapter06_assembler;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
