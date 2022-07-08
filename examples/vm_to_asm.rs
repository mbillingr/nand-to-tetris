/**
Read vm code from a file or directory (multiple .vm files)
and write unoptimized assembly code to stdout.
**/
use nand_to_tetris::chapter08_vm::translator::CodeGenerator;
use std::env;
use std::fs;
use std::path::PathBuf;

fn main() -> Result<(), String> {
    let mut args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("VM to assembly translator.\nUsage:\n    vm_to_asm <input-path>");
        return Ok(());
    }
    let path = args.pop().unwrap();

    let mut input_files = vec![];
    if fs::metadata(&path)
        .map_err(|e| e.to_string() + ": " + &path)?
        .is_file()
    {
        input_files.push(PathBuf::from(path));
    } else {
        for path in fs::read_dir(path)
            .unwrap()
            .map(|p| p.unwrap().path())
            .filter(|p| {
                p.extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| ext.to_lowercase())
                    .map(|ext| ext == "vm")
                    .unwrap_or(false)
            })
        {
            input_files.push(path);
        }
    }

    let mut code_gen = CodeGenerator::new("VM");
    println!("{}", code_gen.gen_bootstrap());

    for file_path in input_files {
        let buffer = fs::read_to_string(&file_path).map_err(|e| e.to_string())?;

        let vm_instructions = CodeGenerator::parse(&buffer);

        let mod_name = file_path.file_stem().unwrap().to_str().unwrap();
        code_gen.set_module_name(mod_name);

        println!("// ==========================================");
        println!("//     Module {}", mod_name);
        println!("// ==========================================");
        println!("{}", code_gen.translate(vm_instructions)?);
    }

    Ok(())
}
