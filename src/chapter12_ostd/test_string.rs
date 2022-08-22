use crate::chapter12_ostd::eval;

#[test]
fn test_string_len() {
    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_new_string_has_zero_length",
            &[]
        ),
        Ok(0)
    );

    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_appending_characters_increases_length",
            &[]
        ),
        Ok(0)
    );

    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_erasing_characters_decreases_length",
            &[]
        ),
        Ok(0)
    );

    assert_eq!(
        eval(&["std/TestString"], "TestString.test_get_max_length", &[]),
        Ok(0)
    );
}

#[test]
fn string_chars() {
    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_can_get_characters_after_appending",
            &[]
        ),
        Ok(0)
    );
    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_can_get_characters_from_literal_string",
            &[]
        ),
        Ok(0)
    );

    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_can_set_characters",
            &[]
        ),
        Ok(0)
    );

    assert_eq!(eval(&[], "String.backSpace", &[]), Ok(129));
    assert_eq!(eval(&[], "String.doubleQuote", &[]), Ok('"' as u16));
    assert_eq!(eval(&[], "String.newLine", &[]), Ok(128));
}

#[test]
fn string_growing() {
    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_resizing_changes_max_length",
            &[]
        ),
        Ok(0)
    );

    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_appending_beyond_max_length_grows_the_string",
            &[]
        ),
        Ok(0)
    );

    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_growing_preserves_content",
            &[]
        ),
        Ok(0)
    );

    assert_eq!(
        eval(
            &["std/TestString"],
            "TestString.test_shrinking_truncates_content",
            &[]
        ),
        Ok(0)
    );
}

#[test]
fn str_to_int() {
    assert_eq!(
        eval(&["std/TestString"], "TestString.test_str_to_int", &[]),
        Ok(0)
    );
}

#[test]
fn int_to_str() {
    assert_eq!(
        eval(&["std/TestString"], "TestString.test_int_to_str", &[]),
        Ok(0)
    );
}
