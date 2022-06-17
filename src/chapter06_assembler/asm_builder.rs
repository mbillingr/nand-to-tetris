#[macro_export]
macro_rules! asm {
    ($(($($stmt:tt)*));* $(;)?) => {
        concat!($(
            concat!(
                asm!(@comment $($stmt)*),
                asm!(@op $($stmt)*),
            )
        ),*)
    };

    (@op goto $target:tt) => {
        concat!($crate::asm_rval!($target), "0;JMP\n")
    };

    (@op $dst:tt = [0]) => {
        concat!($crate::asm_lval!($dst), "M=0\n")
    };

    (@op $dst:tt = [1]) => {
        concat!($crate::asm_lval!($dst), "M=1\n")
    };

    (@op $dst:tt = [-1]) => {
        concat!($crate::asm_lval!($dst), "M=-1\n")
    };

    (@op $dst:tt = [$k:tt]) => {
        concat!("@", $k, "\nD=A\n", $crate::asm_lval!($dst), "M=D\n")
    };

    (@op $dst:tt = $src:tt) => {
        concat!(
            $crate::asm_rval!($src), "D=A\n", $crate::asm_lval!($dst), "M=D\n"
        )
    };

    (@comment $($stmt:tt)*) => {
        concat!(
            "//", $(" ", stringify!($stmt)),*, "\n",
        )
    };
}

#[macro_export]
macro_rules! asm_rval {
    // unwrap parenthesis
    (($($inner:tt)+)) => {
        $crate::asm_rval!($($inner)*)
    };

    // deref operation
    (* $operand:tt) => {
        concat!($crate::asm_rval!($operand), "A=M\n")
    };

    // pre-increment
    (++ $inner:tt) => {
        // inner must be an l-value
        concat!($crate::asm_lval!($inner), "AM=M+1\n")
    };

    // pre-decrement
    (-- $inner:tt) => {
        // inner must be an l-value
        concat!($crate::asm_lval!($inner), "AM=M-1\n")
    };

    // simple increment
    ($left:tt + 1) => {
        concat!($crate::asm_rval!($left), "A=A+1\n")
    };

    // increment optimization #0
    ($left:tt + 0) => {
        $crate::asm_rval!($left)
    };

    // simple increment #2
    ($left:tt + 2) => {
        asm_rval!(($left + 1) + 1)
    };

    // generic increment (only allowed on right hand side because it uses D)
    ($left:tt + $k:tt) => {
        concat!("@", $k, "\nD=A\n", $crate::asm_rval!($left), "A=A+D\n")
    };

    // simple decrement
    ($left:tt - 1) => {
        concat!($crate::asm_rval!($left), "A=A-1\n")
    };

    // decrement optimization #0
    ($left:tt - 0) => {
        $crate::asm_rval!($left)
    };

    // simple decrement #2
    ($left:tt - 2) => {
        $crate::asm_rval!(($left - 1) - 1)
    };

    // generic decrement (only allowed on right hand side because it uses D)
    ($left:tt - $k:tt) => {
        concat!("@", $k, "\nD=A\n", $crate::asm_rval!($left), "A=A-D\n")
    };

    // symbolic identifier (variable name)
    ($var:ident) => {
        concat!("@", stringify!($var), "\nA=M\n")
    };

    // constant
    ($c:tt) => {
        concat!("@", $c, "\n")
    };
}

#[macro_export]
macro_rules! asm_lval {
    // unwrap parenthesis
    (($($inner:tt)+)) => {
        $crate::asm_lval!($($inner)*)
    };

    // deref operation
    (* $operand:tt) => {
        concat!($crate::asm_lval!($operand), "A=M\n")
    };

    // simple increment
    ($left:tt + 1) => {
        concat!($crate::asm_lval!($left), "A=A+1\n")
    };

    // increment optimization #0
    ($left:tt + 0) => {
        $crate::asm_lval!($left)
    };

    // simple increment #2
    ($left:tt + 2) => {
        $crate::asm_lval!(($left + 1) + 1)
    };

    // simple decrement
    ($left:tt - 1) => {
        concat!($crate::asm_lval!($left), "A=A-1\n")
    };

    // decrement optimization #0
    ($left:tt - 0) => {
        $crate::asm_lval!($left)
    };

    // simple decrement #2
    ($left:tt - 2) => {
        $crate::asm_lval!(($left - 1) - 1)
    };

    // symbolic identifier (variable name)
    ($name:ident) => {
        concat!("@", stringify!($name), "\n")
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn variable_assignment() {
        assert_eq!(
            asm!((foo = bar)),
            "// foo = bar\n@bar\nA=M\nD=A\n@foo\nM=D\n"
        );
    }

    #[test]
    fn variable_derefencing() {
        assert_eq!(asm!(@op foo = (*bar)), "@bar\nA=M\nA=M\nD=A\n@foo\nM=D\n");
        assert_eq!(asm!(@op (*foo) = bar), "@bar\nA=M\nD=A\n@foo\nA=M\nM=D\n");
        assert_eq!(
            asm!(@op (*foo) = (*bar)),
            "@bar\nA=M\nA=M\nD=A\n@foo\nA=M\nM=D\n"
        );
        assert_eq!(
            asm!(@op foo = (*(*bar))),
            "@bar\nA=M\nA=M\nA=M\nD=A\n@foo\nM=D\n"
        );
        assert_eq!(
            asm!(@op (*(*foo)) = bar),
            "@bar\nA=M\nD=A\n@foo\nA=M\nA=M\nM=D\n"
        );
    }

    #[test]
    fn pointer_arithmetic() {
        assert_eq!(
            asm!(@op foo = (bar + 1)),
            "@bar\nA=M\nA=A+1\nD=A\n@foo\nM=D\n"
        );
        assert_eq!(
            asm!(@op foo = (*(bar + 1))),
            "@bar\nA=M\nA=A+1\nA=M\nD=A\n@foo\nM=D\n"
        );
        assert_eq!(
            asm!(@op foo = ((*bar) + 1)),
            "@bar\nA=M\nA=M\nA=A+1\nD=A\n@foo\nM=D\n"
        );
        assert_eq!(
            asm!(@op (foo + 1) = bar),
            "@bar\nA=M\nD=A\n@foo\nA=A+1\nM=D\n"
        );
        assert_eq!(
            asm!(@op (*(foo + 1)) = bar),
            "@bar\nA=M\nD=A\n@foo\nA=A+1\nA=M\nM=D\n"
        );
    }

    #[test]
    fn rhs_arithmetic() {
        assert_eq!(asm_rval!(bar + 1), "@bar\nA=M\nA=A+1\n");
        assert_eq!(asm_rval!(bar - 1), "@bar\nA=M\nA=A-1\n");

        assert_eq!(asm_rval!(bar + 0), "@bar\nA=M\n");
        assert_eq!(asm_rval!(bar - 0), "@bar\nA=M\n");

        assert_eq!(asm_rval!(bar + 2), "@bar\nA=M\nA=A+1\nA=A+1\n");
        assert_eq!(asm_rval!(bar - 2), "@bar\nA=M\nA=A-1\nA=A-1\n");

        assert_eq!(asm_rval!(bar + 5), "@5\nD=A\n@bar\nA=M\nA=A+D\n");
        assert_eq!(asm_rval!(bar - 5), "@5\nD=A\n@bar\nA=M\nA=A-D\n");
        //asm_rval!(bar + 5);  // should not compile
        //asm_rval!(bar - 5);  // should not compile
    }

    #[test]
    fn lhs_arithmetic() {
        assert_eq!(asm_lval!(bar + 1), "@bar\nA=A+1\n");
        assert_eq!(asm_lval!(bar - 1), "@bar\nA=A-1\n");

        assert_eq!(asm_lval!(bar + 0), "@bar\n");
        assert_eq!(asm_lval!(bar - 0), "@bar\n");

        assert_eq!(asm_lval!(bar + 2), "@bar\nA=A+1\nA=A+1\n");
        assert_eq!(asm_lval!(bar - 2), "@bar\nA=A-1\nA=A-1\n");

        //asm_lval!(bar + 5);  // should not compile
        //asm_lval!(bar - 5);  // should not compile
    }

    #[test]
    fn constants() {
        assert_eq!(asm!(@op x = 0), "@0\nD=A\n@x\nM=D\n");
        assert_eq!(asm!(@op x = 1), "@1\nD=A\n@x\nM=D\n");
        assert_eq!(asm!(@op x = [-1]), "@x\nM=-1\n");
        assert_eq!(asm!(@op x = 2), "@2\nD=A\n@x\nM=D\n");
    }

    #[test]
    fn multiple_instructions() {
        assert_eq!(
            asm!((SP = 0); ((*SP) = 42); (SP = (SP + 1))),
            concat!(asm!((SP = 0)), asm!(((*SP) = 42)), asm!((SP = (SP + 1))))
        );
    }

    #[test]
    fn goto() {
        assert_eq!(asm!((goto hell)), "// goto hell\n@hell\nA=M\n0;JMP\n")
    }

    #[test]
    fn in_place_arithmetic() {
        assert_eq!(asm_rval!(--y), "@y\nAM=M-1\n");
        assert_eq!(
            asm!((x = (++ y))),
            "// x = (+ + y)\n@y\nAM=M+1\nD=A\n@x\nM=D\n"
        );
    }
}
