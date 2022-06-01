use std::fmt::{Display, Formatter};
use std::iter::Peekable;

pub struct Parser<'s> {
    remaining_lines: Peekable<Box<dyn 's + Iterator<Item = (usize, &'s str)>>>,
    current_line: usize,
    current_instruction: Option<&'s str>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        let code_lines: Box<dyn Iterator<Item = _>> = Box::new(
            source
                .lines()
                .map(|line| line.trim())
                .enumerate()
                .map(|(nr, line)| (nr + 1, line))
                .filter(Self::is_code_line),
        );
        Parser {
            remaining_lines: code_lines.peekable(),
            current_line: 0,
            current_instruction: None,
        }
    }

    pub fn has_more_lines(&mut self) -> bool {
        self.remaining_lines.peek().is_some()
    }

    pub fn advance(&mut self) {
        let (nr, line) = unzip(self.remaining_lines.next());
        self.current_instruction = line;

        if let Some(nr) = nr {
            self.current_line = nr;
        }
    }

    pub fn instruction(&self) -> Result<Instruction, String> {
        let instruction = self
            .current_instruction
            .ok_or_else(|| "unexpected end of input".into())
            .and_then(Instruction::parse);
        self.add_line_to_error(instruction)
    }

    pub fn add_line_to_error<T>(&self, r: Result<T, String>) -> Result<T, String> {
        r.map_err(|msg| format!("line {}: {}", self.current_line, msg))
    }

    fn is_code_line((_, line): &(usize, &str)) -> bool {
        !line.is_empty() && !line.starts_with("//")
    }
}

fn unzip<A, B>(input: Option<(A, B)>) -> (Option<A>, Option<B>) {
    match input {
        Some((a, b)) => (Some(a), Some(b)),
        None => (None, None),
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction<'s> {
    A(&'s str),
    L(&'s str),
    C(Dest, Comp, Jump),
}

impl<'s> Instruction<'s> {
    fn parse(src: &'s str) -> Result<Self, String> {
        if src.starts_with('@') {
            return Ok(Instruction::A(&src[1..]));
        }

        if src.starts_with('(') && src.ends_with(')') {
            return Ok(Instruction::L(&src[1..src.len() - 1]));
        }

        let (dest, rest) = src.split_once('=').unwrap_or(("", src));
        let (comp, jump) = rest.split_once(';').unwrap_or((rest, ""));

        let dest =
            Dest::parse(dest.trim()).ok_or_else(|| format!("invalid destination {}", dest))?;
        let comp =
            Comp::parse(comp.trim()).ok_or_else(|| format!("invalid computation {}", comp))?;
        let jump =
            Jump::parse(jump.trim()).ok_or_else(|| format!("invalid jump condition {}", jump))?;

        Ok(Instruction::C(dest, comp, jump))
    }

    pub fn symbol(&self) -> Option<&'s str> {
        match self {
            Instruction::A(s) | Instruction::L(s) => Some(s),
            _ => None,
        }
    }

    pub fn dest(&self) -> Option<Dest> {
        match self {
            Instruction::C(dest, _, _) => Some(*dest),
            _ => None,
        }
    }

    pub fn comp(&self) -> Option<Comp> {
        match self {
            Instruction::C(_, comp, _) => Some(*comp),
            _ => None,
        }
    }

    pub fn jump(&self) -> Option<Jump> {
        match self {
            Instruction::C(_, _, jump) => Some(*jump),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Jump {
    None,
    Unconditional,
    GreaterZero,
    EqualZero,
    NotNegative,
    Negative,
    NotZero,
    NegativeOrZero,
}

impl Jump {
    fn parse(src: &str) -> Option<Self> {
        match src {
            "" => Some(Jump::None),
            "JMP" => Some(Jump::Unconditional),
            "JGT" => Some(Jump::GreaterZero),
            "JEQ" => Some(Jump::EqualZero),
            "JGE" => Some(Jump::NotNegative),
            "JLT" => Some(Jump::Negative),
            "JNE" => Some(Jump::NotZero),
            "JLE" => Some(Jump::NegativeOrZero),
            _ => None,
        }
    }

    fn to_str(&self) -> &'static str {
        match self {
            Jump::None => "",
            Jump::Unconditional => "JMP",
            Jump::GreaterZero => "JGT",
            Jump::EqualZero => "JEQ",
            Jump::NotNegative => "JGE",
            Jump::Negative => "JLT",
            Jump::NotZero => "JNE",
            Jump::NegativeOrZero => "JLE",
        }
    }
}

impl Display for Comp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.to_str().fmt(f)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Dest {
    None,
    A,
    D,
    M,
    AD,
    AM,
    DM,
    ADM,
}

impl Dest {
    fn parse(src: &str) -> Option<Self> {
        let (mut a, mut d, mut m, mut other) = (0, 0, 0, 0);
        for ch in src.chars() {
            match ch {
                'A' => a += 1,
                'D' => d += 1,
                'M' => m += 1,
                _ => other += 1,
            }
        }

        match (a, d, m, other) {
            (0, 0, 0, 0) => Some(Dest::None),
            (1, 0, 0, 0) => Some(Dest::A),
            (0, 1, 0, 0) => Some(Dest::D),
            (1, 1, 0, 0) => Some(Dest::AD),
            (0, 0, 1, 0) => Some(Dest::M),
            (1, 0, 1, 0) => Some(Dest::AM),
            (0, 1, 1, 0) => Some(Dest::DM),
            (1, 1, 1, 0) => Some(Dest::ADM),
            _ => None,
        }
    }

    fn to_str(&self) -> &'static str {
        match self {
            Dest::None => "",
            Dest::A => "A",
            Dest::D => "D",
            Dest::M => "M",
            Dest::AD => "AD",
            Dest::AM => "AM",
            Dest::DM => "DM",
            Dest::ADM => "ADM",
        }
    }
}

impl Display for Dest {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.to_str().fmt(f)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Comp {
    Zero,
    One,
    MinusOne,
    D,
    A,
    M,
    NotD,
    NotA,
    NotM,
    NegD,
    NegA,
    NegM,
    IncD,
    IncA,
    IncM,
    DecD,
    DecA,
    DecM,
    DplusA,
    DplusM,
    DminusA,
    DminusM,
    AminusD,
    MminusD,
    DandA,
    DandM,
    DorA,
    DorM,
}

impl Comp {
    fn parse(src: &str) -> Option<Self> {
        match src {
            "0" => Some(Comp::Zero),
            "1" => Some(Comp::One),
            "-1" => Some(Comp::MinusOne),
            "D" => Some(Comp::D),
            "A" => Some(Comp::A),
            "M" => Some(Comp::M),
            "!D" => Some(Comp::NotD),
            "!A" => Some(Comp::NotA),
            "!M" => Some(Comp::NotM),
            "-D" => Some(Comp::NegD),
            "-A" => Some(Comp::NegA),
            "-M" => Some(Comp::NegM),
            "D+1" | "1+D" => Some(Comp::IncD),
            "A+1" | "1+A" => Some(Comp::IncA),
            "M+1" | "1+M" => Some(Comp::IncM),
            "D-1" => Some(Comp::DecD),
            "A-1" => Some(Comp::DecA),
            "M-1" => Some(Comp::DecM),
            "D+A" | "A+D" => Some(Comp::DplusA),
            "D+M" | "M+D" => Some(Comp::DplusM),
            "D-A" => Some(Comp::DminusA),
            "D-M" => Some(Comp::DminusM),
            "A-D" => Some(Comp::AminusD),
            "M-D" => Some(Comp::MminusD),
            "D&A" | "A&D" => Some(Comp::DandA),
            "D&M" | "M&D" => Some(Comp::DandM),
            "D|A" | "A|D" => Some(Comp::DorA),
            "D|M" | "M|D" => Some(Comp::DorM),
            _ => None,
        }
    }

    fn to_str(&self) -> &'static str {
        match self {
            Comp::Zero => "0",
            Comp::One => "1",
            Comp::MinusOne => "-1",
            Comp::D => "D",
            Comp::A => "A",
            Comp::M => "M",
            Comp::NotD => "!D",
            Comp::NotA => "!A",
            Comp::NotM => "!M",
            Comp::NegD => "-D",
            Comp::NegA => "-A",
            Comp::NegM => "-M",
            Comp::IncD => "D+1",
            Comp::IncA => "A+1",
            Comp::IncM => "M+1",
            Comp::DecD => "D-1",
            Comp::DecA => "A-1",
            Comp::DecM => "M-1",
            Comp::DplusA => "D+A",
            Comp::DplusM => "D+M",
            Comp::DminusA => "D-A",
            Comp::DminusM => "D-M",
            Comp::AminusD => "A-D",
            Comp::MminusD => "M-D",
            Comp::DandA => "D&A",
            Comp::DandM => "D&M",
            Comp::DorA => "D|A",
            Comp::DorM => "D|M",
        }
    }
}

impl Display for Jump {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.to_str().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_more_lines_in_empty_input() {
        assert_eq!(Parser::new("").has_more_lines(), false);
    }

    #[test]
    fn more_lines_in_single_line_input() {
        assert_eq!(Parser::new("foobar").has_more_lines(), true);
    }

    #[test]
    fn advance_sets_current_instruction() {
        let mut parser = Parser::new("command");
        assert_eq!(parser.current_instruction, None);
        parser.advance();
        assert_eq!(parser.current_instruction, Some("command"));
    }

    #[test]
    fn advancing_past_the_last_instruction() {
        let mut parser = Parser::new("command");
        parser.advance();
        assert_eq!(parser.has_more_lines(), false);
    }

    #[test]
    fn advance_skips_empty_lines() {
        let mut parser = Parser::new("cmd1\n\n\ncmd2");
        parser.advance();
        parser.advance();
        assert_eq!(parser.current_instruction, Some("cmd2"));
    }

    #[test]
    fn advance_skips_comment_lines() {
        let mut parser = Parser::new("cmd1\n//comment\ncmd2");
        parser.advance();
        parser.advance();
        assert_eq!(parser.current_instruction, Some("cmd2"));
    }

    #[test]
    fn no_more_lines_if_last_is_comment() {
        let mut parser = Parser::new("command\n//comment");
        parser.advance();
        assert_eq!(parser.has_more_lines(), false);
    }

    #[test]
    fn skip_leading_comments() {
        let mut parser = Parser::new("//comment\ncommand");
        parser.advance();
        assert_eq!(parser.current_instruction, Some("command"));
    }

    #[test]
    fn skip_whitespace_only_lines() {
        let mut parser = Parser::new("\t  ");
        assert_eq!(parser.has_more_lines(), false);
    }

    #[test]
    fn skip_comments_starting_with_whitespace() {
        let mut parser = Parser::new("\t  //comment");
        assert_eq!(parser.has_more_lines(), false);
    }

    #[test]
    fn whitespace_trimmed_from_instructions() {
        let mut parser = Parser::new("\tcommand    ");
        parser.advance();
        assert_eq!(parser.current_instruction, Some("command"));
    }

    #[test]
    fn instruction_method_requires_current_instruction() {
        assert_eq!(
            Parser::new("").instruction(),
            Err("line 0: unexpected end of input".to_string())
        );
    }

    #[test]
    fn parse_constants() {
        let mut parser = Parser::new("@xyz");
        parser.advance();
        assert_eq!(parser.instruction(), Ok(Instruction::A("xyz")));
    }

    #[test]
    fn parse_labels() {
        let mut parser = Parser::new("(xyz)");
        parser.advance();
        assert_eq!(parser.instruction(), Ok(Instruction::L("xyz")));
    }

    #[test]
    fn parse_instructions() {
        let mut parser = Parser::new("A=0;JNE");
        parser.advance();
        assert_eq!(
            parser.instruction(),
            Ok(Instruction::C(Dest::A, Comp::Zero, Jump::NotZero))
        );
    }

    #[test]
    fn parse_errors_report_line_numbers() {
        let mut parser = Parser::new("//comment\n=;");
        parser.advance();
        assert!(parser.instruction().unwrap_err().starts_with("line 2"));
    }

    #[test]
    fn construct_full_c_instruction() {
        assert_eq!(
            Instruction::parse("A=1;JMP"),
            Ok(Instruction::C(Dest::A, Comp::One, Jump::Unconditional))
        );
    }

    #[test]
    fn construct_minimal_c_instruction() {
        assert_eq!(
            Instruction::parse("0"),
            Ok(Instruction::C(Dest::None, Comp::Zero, Jump::None))
        );
    }

    #[test]
    fn construct_jump_instruction() {
        assert_eq!(
            Instruction::parse("0;JMP"),
            Ok(Instruction::C(Dest::None, Comp::Zero, Jump::Unconditional))
        );
    }

    #[test]
    fn construct_assignment_instruction() {
        assert_eq!(
            Instruction::parse("A=1"),
            Ok(Instruction::C(Dest::A, Comp::One, Jump::None))
        );
    }

    #[test]
    fn construct_invalid_computation() {
        assert_eq!(
            Instruction::parse("x*y"),
            Err("invalid computation x*y".to_string())
        );
    }

    #[test]
    fn construct_invalid_destination() {
        assert_eq!(
            Instruction::parse("x=0"),
            Err("invalid destination x".to_string())
        );
    }

    #[test]
    fn construct_invalid_jump() {
        assert_eq!(
            Instruction::parse("0;BRANCH"),
            Err("invalid jump condition BRANCH".to_string())
        );
    }

    #[test]
    fn canonical_computation_mnemonics() {
        let computations = [
            ("0", Comp::Zero),
            ("1", Comp::One),
            ("-1", Comp::MinusOne),
            ("D", Comp::D),
            ("A", Comp::A),
            ("M", Comp::M),
            ("!D", Comp::NotD),
            ("!A", Comp::NotA),
            ("!M", Comp::NotM),
            ("-D", Comp::NegD),
            ("-A", Comp::NegA),
            ("-M", Comp::NegM),
            ("D+1", Comp::IncD),
            ("A+1", Comp::IncA),
            ("M+1", Comp::IncM),
            ("D-1", Comp::DecD),
            ("A-1", Comp::DecA),
            ("M-1", Comp::DecM),
            ("D+A", Comp::DplusA),
            ("D+M", Comp::DplusM),
            ("D-A", Comp::DminusA),
            ("D-M", Comp::DminusM),
            ("A-D", Comp::AminusD),
            ("M-D", Comp::MminusD),
            ("D&A", Comp::DandA),
            ("D&M", Comp::DandM),
            ("D|A", Comp::DorA),
            ("D|M", Comp::DorM),
        ];

        for (comp_str, comp_enum) in computations {
            let parsed = Comp::parse(comp_str);
            if parsed != Some(comp_enum) {
                panic!(
                    "\"{}\" parses to {:?} instead of {:?}",
                    comp_str,
                    parsed,
                    Some(comp_enum)
                )
            }

            let string = comp_enum.to_str();
            if string != comp_str {
                panic!(
                    "{:?} prints as {:?} instead of {:?}",
                    comp_enum, string, comp_str
                )
            }
        }
    }

    #[test]
    fn alternative_computation_mnemonics() {
        let computations = [
            ("1+D", Comp::IncD),
            ("1+A", Comp::IncA),
            ("1+M", Comp::IncM),
            ("A+D", Comp::DplusA),
            ("M+D", Comp::DplusM),
            ("A&D", Comp::DandA),
            ("M&D", Comp::DandM),
            ("A|D", Comp::DorA),
            ("M|D", Comp::DorM),
        ];

        for (comp_str, comp_enum) in computations {
            let parsed = Comp::parse(comp_str);
            if parsed != Some(comp_enum) {
                panic!(
                    "\"{}\" parses to {:?} instead of {:?}",
                    comp_str,
                    parsed,
                    Some(comp_enum)
                )
            }
        }
    }

    #[test]
    fn canonical_jump_mnemonics() {
        let jumps = [
            ("", Jump::None),
            ("JMP", Jump::Unconditional),
            ("JGT", Jump::GreaterZero),
            ("JEQ", Jump::EqualZero),
            ("JGE", Jump::NotNegative),
            ("JLT", Jump::Negative),
            ("JNE", Jump::NotZero),
            ("JLE", Jump::NegativeOrZero),
        ];

        for (jump_str, jump_enum) in jumps {
            let parsed = Jump::parse(jump_str);
            if parsed != Some(jump_enum) {
                panic!(
                    "\"{}\" parses to {:?} instead of {:?}",
                    jump_str,
                    parsed,
                    Some(jump_enum)
                )
            }

            let string = jump_enum.to_str();
            if string != jump_str {
                panic!(
                    "{:?} prints as {:?} instead of {:?}",
                    jump_enum, string, jump_str
                )
            }
        }
    }

    #[test]
    fn canonical_dest_mnemonics() {
        let dests = [
            ("", Dest::None),
            ("A", Dest::A),
            ("AD", Dest::AD),
            ("ADM", Dest::ADM),
            ("AM", Dest::AM),
            ("D", Dest::D),
            ("DM", Dest::DM),
            ("M", Dest::M),
        ];

        for (dest_str, dest_enum) in dests {
            let parsed = Dest::parse(dest_str);
            if parsed != Some(dest_enum) {
                panic!(
                    "\"{}\" parses to {:?} instead of {:?}",
                    dest_str,
                    parsed,
                    Some(dest_enum)
                )
            }

            let string = dest_enum.to_str();
            if string != dest_str {
                panic!(
                    "{:?} prints as {:?} instead of {:?}",
                    dest_enum, string, dest_str
                )
            }
        }
    }
}
