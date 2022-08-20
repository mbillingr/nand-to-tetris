use crate::chapter12_ostd::eval;

#[test]
fn test_peek_and_poke() {
    assert_eq!(eval(&[], "Memory.peek", &[0]), Ok(268)); // where the stack pointer happens to be...
    assert_eq!(
        eval(&["std/TestMemory"], "TestMemory.test_peekpoke", &[]),
        Ok(123)
    );
}

#[test]
fn test_init() {
    assert_eq!(eval(&[], "Memory._get_freelist", &[]), Ok(2048));
}

#[test]
fn test_alloc() {
    assert_eq!(eval(&[], "Memory.alloc", &[42]), Ok(2049));
    assert_eq!(
        eval(&["std/TestMemory"], "TestMemory.test_alloc", &[]),
        Ok(0)
    );
}
