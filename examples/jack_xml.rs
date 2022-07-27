/**
Read vm code from a file or directory (multiple .vm files)
and write unoptimized assembly code to stdout.
**/
use nand_to_tetris::chapter10_compiler::parser;
use nand_to_tetris::chapter10_compiler::parser::ClassVarDec;
use nand_to_tetris::chapter10_compiler::tokenizer::JackTokenizer;
use nand_to_tetris::file_utils::file_or_files_in_dir;
use std::env;
use std::fs;

fn main() -> Result<(), String> {
    let mut args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Jack Analyzer.\nUsage:\n    jack_xml <input-path>");
        return Ok(());
    }
    let path = args.pop().unwrap();

    let input_files = file_or_files_in_dir(&path, "jack")?;

    for input_file in input_files {
        let buffer = fs::read_to_string(&input_file).map_err(|e| e.to_string())?;
        let lexer = JackTokenizer::new(&buffer);

        let parse_tree = parser::class(lexer).map_err(|e| format!("{:?}", e))?.0;
        let xmlstr = parse_tree.to_xml();
        println!("{}", xmlstr);

        let output_file = input_file.with_extension("xml");
    }

    Ok(())
}

trait ToXml {
    fn to_xml(&self) -> String {
        self.xml_indent("")
    }

    fn xml_indent(&self, level: &str) -> String;
}

impl ToXml for parser::Class {
    fn xml_indent(&self, level0: &str) -> String {
        let level1 = format!("{level0}    ");
        let mut out = format!(
            "{level0}<class>\n\
             {level1}<name> {name} </name>\n",
            name = self.name
        );
        for vdec in &self.vars {
            out += &vdec.xml_indent(&level1);
            out += "\n";
        }
        for fun in &self.funs {
            out += &fun.xml_indent(&level1);
            out += "\n";
        }
        out += &format!("{level0}</class>");
        out
    }
}

impl ToXml for parser::ClassVarDec {
    fn xml_indent(&self, level0: &str) -> String {
        let level1 = format!("{level0}    ");
        let mut out = format!("{level0}<classVarDec>\n",);
        let (k, t, vs) = match self {
            ClassVarDec::Static(t, vs) => ("static", t, vs),
            ClassVarDec::Field(t, vs) => ("field", t, vs),
        };
        out += &format!("{level1}<kind> {k} </kind>\n");
        out += &format!("{level1}<type> {t} </type>\n");
        for name in vs {
            out += &format!("{level1}<name> {name} </name>\n");
        }
        out += &format!("{level0}</classVarDec>");
        out
    }
}

impl ToXml for parser::SubroutineDec {
    fn xml_indent(&self, level0: &str) -> String {
        let level1 = format!("{level0}    ");
        let mut out = format!("{level0}<subroutineDec>\n",);
        out += &format!("{level1}TODO\n");
        todo!();
        out += &format!("{level0}</subroutineDec>");
        out
    }
}
