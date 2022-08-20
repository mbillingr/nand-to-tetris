use crate::chapter12_ostd::eval;

const RAM_END: u16 = 0x4000;

#[test]
fn test_peek_and_poke() {
    assert_eq!(eval(&[], "Memory.peek", &[0]), Ok(273)); // where the stack pointer happens to be...
    assert_eq!(
        eval(&["std/TestMemory"], "TestMemory.test_peekpoke", &[]),
        Ok(123)
    );
}

#[test]
fn test_init() {
    assert_eq!(eval(&[], "Memory._get_freelist", &[]), Ok(2048));
    assert_eq!(eval(&[], "Memory.peek", &[2048]), Ok(0));
    assert_eq!(eval(&[], "Memory.peek", &[2049]), Ok(2050));
    assert_eq!(eval(&[], "Memory.peek", &[2050]), Ok(16384 - 2050));
    assert_eq!(eval(&[], "Memory.peek", &[2051]), Ok(0));
}

#[test]
fn test_alloc() {
    assert_eq!(eval(&[], "Memory.alloc", &[42]), Ok(RAM_END - 42));
    assert_eq!(
        eval(
            &["std/TestMemory"],
            "TestMemory.test_alloc_partial_segment",
            &[]
        ),
        Ok(0)
    );
    assert_eq!(
        eval(
            &["std/TestMemory"],
            "TestMemory.test_alloc_exact_segment",
            &[]
        ),
        Ok(0)
    );
}

#[test]
fn test_dealloc() {
    assert_eq!(
        eval(&["std/TestMemory"], "TestMemory.test_dealloc", &[]),
        Ok(0)
    );
}
