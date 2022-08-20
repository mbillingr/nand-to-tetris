use crate::chapter12_ostd::eval;

#[test]
fn test_multiply() {
    assert_eq!(eval(&[], "Math.multiply", &[0, 0]), Ok(0));
    assert_eq!(eval(&[], "Math.multiply", &[0, 1]), Ok(0));
    assert_eq!(eval(&[], "Math.multiply", &[1, 0]), Ok(0));
    assert_eq!(eval(&[], "Math.multiply", &[1, 1]), Ok(1));
    assert_eq!(eval(&[], "Math.multiply", &[2, 1]), Ok(2));
    assert_eq!(eval(&[], "Math.multiply", &[1, 2]), Ok(2));
    assert_eq!(eval(&[], "Math.multiply", &[2, 2]), Ok(4));
    assert_eq!(eval(&[], "Math.multiply", &[7, 6]), Ok(42));
    assert_eq!(eval(&[], "Math.multiply", &[256, 255]), Ok(255 * 256));
    assert_eq!(eval(&[], "Math.multiply", &[256, 256]), Ok(0));
    assert_eq!(eval(&[], "Math.multiply", &[256, 257]), Ok(256));
}

#[test]
fn test_divide() {
    assert_eq!(eval(&[], "Math.divide", &[0, 1]), Ok(0));
    assert_eq!(eval(&[], "Math.divide", &[1, 1]), Ok(1));
    assert_eq!(eval(&[], "Math.divide", &[2, 1]), Ok(2));
    assert_eq!(eval(&[], "Math.divide", &[2, 2]), Ok(1));
    assert_eq!(eval(&[], "Math.divide", &[3, 2]), Ok(1));
    assert_eq!(eval(&[], "Math.divide", &[4, 2]), Ok(2));
    assert_eq!(eval(&[], "Math.divide", &[1234, 9]), Ok(1234 / 9));
    assert_eq!(eval(&[], "Math.divide", &[1230, 10]), Ok(123));
    assert_eq!(eval(&[], "Math.divide", &[12305, 10]), Ok(1230));
}

#[test]
fn test_sqrt() {
    assert_eq!(eval(&[], "Math.sqrt", &[0]), Ok(0));
    assert_eq!(eval(&[], "Math.sqrt", &[2]), Ok(1));
    assert_eq!(eval(&[], "Math.sqrt", &[3]), Ok(1));
    assert_eq!(eval(&[], "Math.sqrt", &[4]), Ok(2));
    assert_eq!(eval(&[], "Math.sqrt", &[9]), Ok(3));
    assert_eq!(eval(&[], "Math.sqrt", &[15]), Ok(3));
    assert_eq!(eval(&[], "Math.sqrt", &[16]), Ok(4));
    assert_eq!(eval(&[], "Math.sqrt", &[17]), Ok(4));
    assert_eq!(eval(&[], "Math.sqrt", &[10000]), Ok(100));
    assert_eq!(eval(&[], "Math.sqrt", &[16384]), Ok(128));
}

#[test]
fn test_min() {
    assert_eq!(eval(&[], "Math.min", &[0, 0]), Ok(0));
    assert_eq!(eval(&[], "Math.min", &[1, 2]), Ok(1));
    assert_eq!(eval(&[], "Math.min", &[3, 2]), Ok(2));
}

#[test]
fn test_max() {
    assert_eq!(eval(&[], "Math.max", &[0, 0]), Ok(0));
    assert_eq!(eval(&[], "Math.max", &[1, 2]), Ok(2));
    assert_eq!(eval(&[], "Math.max", &[3, 2]), Ok(3));
}
