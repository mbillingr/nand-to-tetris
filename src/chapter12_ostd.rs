#[cfg(test)]
mod tests {
    use crate::chapter06_assembler::assembler::{assemble, SP};
    use crate::chapter07_vm::parser::{Command as StackCmd, Segment};
    use crate::chapter08_vm::parser::Command;
    use crate::chapter08_vm::translator::CodeGenerator;
    use crate::chapter11_compiler::compiler::Compiler;
    use crate::computer_emulator::Computer;
    use std::fs;

    fn eval(modules: &[&str], func: &str, args: &[u16]) -> Result<u16, String> {
        let mut asm = String::new();
        let mut code_gen = CodeGenerator::new("VM");
        asm += &code_gen.gen_bootstrap();

        for module in modules {
            let buffer =
                fs::read_to_string(format!("content/{module}")).map_err(|e| e.to_string())?;
            let compiler = Compiler::compile_source(&buffer)?;

            let mod_name = module
                .rsplitn(2, '/')
                .next()
                .unwrap()
                .split('.')
                .next()
                .unwrap();
            code_gen.set_module_name(mod_name);

            asm += &code_gen.translate(compiler.code().iter().copied().map(Ok))?;
        }

        let mut init = vec![Command::Function("Sys.init", 0)];

        for arg in args {
            init.push(Command::Stack(StackCmd::Push(Segment::Constant, *arg)));
        }

        init.push(Command::Call(func, args.len() as u16));
        init.push(Command::Halt);

        asm += &code_gen.translate(init.into_iter().map(Ok))?;

        let binary = assemble(&asm)?;

        let mut emu = Computer::new(binary);

        emu.run(usize::MAX);

        let sp = emu.get_ram(SP);
        Ok(emu.get_ram(sp - 1))
    }

    #[test]
    fn test_multiply() {
        assert_eq!(eval(&["std/Math.jack"], "Math.multiply", &[0, 0]), Ok(0));
        assert_eq!(eval(&["std/Math.jack"], "Math.multiply", &[0, 1]), Ok(0));
        assert_eq!(eval(&["std/Math.jack"], "Math.multiply", &[1, 0]), Ok(0));
        assert_eq!(eval(&["std/Math.jack"], "Math.multiply", &[1, 1]), Ok(1));
        assert_eq!(eval(&["std/Math.jack"], "Math.multiply", &[2, 1]), Ok(2));
        assert_eq!(eval(&["std/Math.jack"], "Math.multiply", &[1, 2]), Ok(2));
        assert_eq!(eval(&["std/Math.jack"], "Math.multiply", &[2, 2]), Ok(4));
        assert_eq!(eval(&["std/Math.jack"], "Math.multiply", &[7, 6]), Ok(42));
        assert_eq!(
            eval(&["std/Math.jack"], "Math.multiply", &[256, 255]),
            Ok(255 * 256)
        );
        assert_eq!(
            eval(&["std/Math.jack"], "Math.multiply", &[256, 256]),
            Ok(0)
        );
        assert_eq!(
            eval(&["std/Math.jack"], "Math.multiply", &[256, 257]),
            Ok(256)
        );
    }

    #[test]
    fn test_divide() {
        assert_eq!(eval(&["std/Math.jack"], "Math.divide", &[0, 1]), Ok(0));
        assert_eq!(eval(&["std/Math.jack"], "Math.divide", &[1, 1]), Ok(1));
        assert_eq!(eval(&["std/Math.jack"], "Math.divide", &[2, 1]), Ok(2));
        assert_eq!(eval(&["std/Math.jack"], "Math.divide", &[2, 2]), Ok(1));
        assert_eq!(eval(&["std/Math.jack"], "Math.divide", &[3, 2]), Ok(1));
        assert_eq!(eval(&["std/Math.jack"], "Math.divide", &[4, 2]), Ok(2));
        assert_eq!(
            eval(&["std/Math.jack"], "Math.divide", &[1234, 9]),
            Ok(1234 / 9)
        );
        assert_eq!(
            eval(&["std/Math.jack"], "Math.divide", &[1230, 10]),
            Ok(123)
        );
        assert_eq!(
            eval(&["std/Math.jack"], "Math.divide", &[12305, 10]),
            Ok(1230)
        );
    }

    #[test]
    fn test_sqrt() {
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[0]), Ok(0));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[2]), Ok(1));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[3]), Ok(1));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[4]), Ok(2));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[9]), Ok(3));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[15]), Ok(3));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[16]), Ok(4));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[17]), Ok(4));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[10000]), Ok(100));
        assert_eq!(eval(&["std/Math.jack"], "Math.sqrt", &[16384]), Ok(128));
    }

    #[test]
    fn test_min() {
        assert_eq!(eval(&["std/Math.jack"], "Math.min", &[0, 0]), Ok(0));
        assert_eq!(eval(&["std/Math.jack"], "Math.min", &[1, 2]), Ok(1));
        assert_eq!(eval(&["std/Math.jack"], "Math.min", &[3, 2]), Ok(2));
    }

    #[test]
    fn test_max() {
        assert_eq!(eval(&["std/Math.jack"], "Math.max", &[0, 0]), Ok(0));
        assert_eq!(eval(&["std/Math.jack"], "Math.max", &[1, 2]), Ok(2));
        assert_eq!(eval(&["std/Math.jack"], "Math.max", &[3, 2]), Ok(3));
    }
}
