use crate::chapter07_vm::parser::Command;

pub fn optimize_pair(first: Command, next: Command) -> Result<Command, (Command, Command)> {
    use Command::*;
    match (first, next) {
        //(Push(src, i), Pop(dst, j)) => Ok(Move(src, i, dst, j)),
        (a, b) => Err((a, b)),
    }
}
