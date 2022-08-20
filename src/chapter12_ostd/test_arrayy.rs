use crate::chapter12_ostd::eval;

#[test]
fn test_array() {
    assert_eq!(eval(&["std/TestArray"], "TestArray.test_array", &[]), Ok(0));
    assert_eq!(
        eval(&["std/TestArray"], "TestArray.test_dispose", &[]),
        Ok(0)
    );
}
