use std::iter::Peekable;

pub struct Parser<'s> {
    remaining_lines: Peekable<Box<dyn 's + Iterator<Item = &'s str>>>,
    current_instruction: Option<&'s str>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        let code_lines: Box<dyn Iterator<Item = _>> = Box::new(
            source
                .lines()
                .map(|line| line.trim())
                .filter(Self::is_code_line),
        );
        Parser {
            remaining_lines: code_lines.peekable(),
            current_instruction: None,
        }
    }

    pub fn has_more_lines(&mut self) -> bool {
        self.remaining_lines.peek().is_some()
    }

    pub fn advance(&mut self) {
        self.current_instruction = self.remaining_lines.next();
    }

    pub fn instruction(&self) -> Result<Instruction, Box<str>> {
        self.current_instruction
            .and_then(|s| Instruction::parse(s))
            .ok_or("parser error".into())
    }

    fn is_code_line(line: &&str) -> bool {
        !line.is_empty() && !line.starts_with("//")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction<'s> {
    A(&'s str),
    L(&'s str),
    C(&'s str, &'s str, &'s str),
}

impl<'s> Instruction<'s> {
    fn parse(src: &'s str) -> Option<Self> {
        if src.starts_with('@') {
            return Some(Instruction::A(&src[1..]));
        }

        if src.starts_with('(') && src.ends_with(')') {
            return Some(Instruction::L(&src[1..src.len() - 1]));
        }

        let (dest, rest) = src.split_once('=').unwrap_or(("", src));
        let (comp, jump) = rest.split_once(';').unwrap_or((rest, ""));
        Some(Instruction::C(dest.trim(), comp.trim(), jump.trim()))
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
            Parser::new("").instruction().unwrap_err().as_ref(),
            "parser error"
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
        let mut parser = Parser::new("dest=comp;jump");
        parser.advance();
        assert_eq!(
            parser.instruction(),
            Ok(Instruction::C("dest", "comp", "jump"))
        );
    }

    #[test]
    fn construct_full_c_instruction() {
        assert_eq!(
            Instruction::parse("ddd=ccc;jjj"),
            Some(Instruction::C("ddd", "ccc", "jjj"))
        );
    }

    #[test]
    fn construct_minimal_c_instruction() {
        assert_eq!(
            Instruction::parse("ccc"),
            Some(Instruction::C("", "ccc", ""))
        );
    }

    #[test]
    fn construct_jump_instruction() {
        assert_eq!(
            Instruction::parse("0;JMP"),
            Some(Instruction::C("", "0", "JMP"))
        );
    }

    #[test]
    fn construct_assignment_instruction() {
        assert_eq!(
            Instruction::parse("x=1"),
            Some(Instruction::C("x", "1", ""))
        );
    }
}
