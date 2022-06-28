use crate::chapter07_vm::parser::Command;

pub fn optimize_pair(first: Command, next: Command) -> Result<Command, (Command, Command)> {
    match (first, next) {
        (a, b) => Err((a, b)),
    }
}
