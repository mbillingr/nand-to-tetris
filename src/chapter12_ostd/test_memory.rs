use crate::chapter12_ostd::eval;

#[test]
fn test_peek_and_poke() {
    assert_eq!(eval(&["std/Memory.jack"], "Memory.peek", &[0]), Ok(268)); // where the stack pointer happens to be...
    assert_eq!(
        eval(&["std/Memory.jack"], "Memory.test_peekpoke", &[0]),
        Ok(123)
    );
}
