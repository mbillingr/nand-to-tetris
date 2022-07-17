#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct JackTokenizer<'s> {
    source: &'s str,
    pub current_token: Token<'s>,
}

impl<'s> JackTokenizer<'s> {
    pub fn new(source: &'s str) -> Self {
        JackTokenizer {
            source,
            current_token: Token::Invalid(""),
        }
        .skip_whitespace()
        .advance()
    }

    /// crate an empty tokenizer
    pub fn end() -> Self {
        JackTokenizer {
            source: "",
            current_token: Token::Eof,
        }
    }

    pub fn has_more_tokens(&self) -> bool {
        true
    }

    pub fn advance(self) -> Self {
        let (token, source) = Self::extract_token(self.source);
        JackTokenizer {
            source,
            current_token: token,
        }
        .skip_whitespace()
    }

    fn skip_whitespace(self) -> Self {
        JackTokenizer {
            source: self.source.trim_start(),
            current_token: self.current_token,
        }
    }

    fn extract_token(src: &'s str) -> (Token<'s>, &'s str) {
        match src.chars().next() {
            Some('"') => Self::extract_string(src),
            Some(ch) if is_symbol_char(ch) => Self::extract_symbol(src),
            Some(ch) if ch.is_digit(10) => Self::extract_integer(src),
            Some(_) => Self::extract_identifier_or_keyword(src),
            None => (Token::Eof, ""),
        }
    }

    fn extract_identifier_or_keyword(src: &'s str) -> (Token<'s>, &'s str) {
        for (idx, ch) in src.char_indices() {
            if !is_identifier_char(ch) {
                return (Token::identifier_or_keyword(&src[..idx]), &src[idx..]);
            }
        }
        (Token::identifier_or_keyword(src), "")
    }

    fn extract_string(src: &'s str) -> (Token<'s>, &'s str) {
        for (idx, ch) in src.char_indices().skip(1) {
            if ch == '"' {
                return (Token::StringConstant(&src[1..idx]), &src[1 + idx..]);
            }
        }
        panic!("Unterminated string")
    }

    fn extract_integer(src: &'s str) -> (Token<'s>, &'s str) {
        for (idx, ch) in src.char_indices().skip(1) {
            if !ch.is_digit(10) {
                return (
                    Token::IntegerConstant(src[..idx].parse().unwrap()),
                    &src[idx..],
                );
            }
        }
        (Token::IntegerConstant(src.parse().unwrap()), "")
    }

    fn extract_symbol(src: &'s str) -> (Token<'s>, &'s str) {
        let mut chars = src.char_indices();
        let (_, ch) = chars.next().unwrap();
        match chars.next() {
            Some((idx, _)) => (Token::Symbol(ch), &src[idx..]),
            None => (Token::Symbol(ch), ""),
        }
    }
}

fn is_identifier_char(ch: char) -> bool {
    !(ch.is_whitespace() | is_symbol_char(ch))
}

fn is_symbol_char(ch: char) -> bool {
    match ch {
        '{' | '}' | '(' | ')' | '[' | ']' | '.' | ',' | ';' | '+' | '-' | '*' | '/' | '&' | '|'
        | '<' | '>' | '=' | '~' => true,
        _ => false,
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Token<'s> {
    Invalid(&'s str),
    Keyword(Keyword),
    Symbol(char),
    IntegerConstant(u16),
    StringConstant(&'s str),
    Identifier(&'s str),
    Eof,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Keyword {
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
}

impl<'s> Token<'s> {
    fn identifier_or_keyword(s: &'s str) -> Self {
        Keyword::try_from_str(s)
            .map(Token::Keyword)
            .unwrap_or_else(|| Token::Identifier(s))
    }
}

impl Keyword {
    fn try_from_str(s: &str) -> Option<Self> {
        match s {
            "class" => Some(Keyword::Class),
            "constructor" => Some(Keyword::Constructor),
            "function" => Some(Keyword::Function),
            "method" => Some(Keyword::Method),
            "field" => Some(Keyword::Field),
            "static" => Some(Keyword::Static),
            "var" => Some(Keyword::Var),
            "int" => Some(Keyword::Int),
            "char" => Some(Keyword::Char),
            "boolean" => Some(Keyword::Boolean),
            "void" => Some(Keyword::Void),
            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),
            "null" => Some(Keyword::Null),
            "this" => Some(Keyword::This),
            "let" => Some(Keyword::Let),
            "do" => Some(Keyword::Do),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "return" => Some(Keyword::Return),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_tokenizer_has_only_eof_tokens() {
        let lexer = JackTokenizer::new("");
        assert!(lexer.has_more_tokens());
        assert_eq!(lexer.current_token, Token::Eof);
        assert_eq!(lexer.advance().current_token, Token::Eof);
    }

    #[test]
    fn nonempty_tokenizer_has_tokens() {
        let lexer = JackTokenizer::new("foo");
        assert!(lexer.has_more_tokens());
    }

    #[test]
    fn whitespace_does_not_count_as_token() {
        let lexer = JackTokenizer::new(" \n ");
        assert_eq!(lexer.current_token, Token::Eof);
    }

    #[test]
    fn tokenizer_starts_at_first_token() {
        let lexer = JackTokenizer::new("foo bar");
        assert_eq!(lexer.current_token, Token::Identifier("foo"));
    }

    #[test]
    fn advance_sets_current_token() {
        let lexer = JackTokenizer::new("foo bar").advance();
        assert_eq!(lexer.current_token, Token::Identifier("bar"));
    }

    #[test]
    fn tokens_separate_by_whitespace() {
        let lexer = JackTokenizer::new("foo  bar");
        assert_eq!(lexer.current_token, Token::Identifier("foo"));
        let lexer = lexer.advance();
        assert_eq!(lexer.current_token, Token::Identifier("bar"));
    }

    #[test]
    fn parse_identifier() {
        let lexer = JackTokenizer::new(" foo ");
        assert_eq!(lexer.current_token, Token::Identifier("foo"));
    }

    #[test]
    fn parse_string() {
        let lexer = JackTokenizer::new(" \"foo\" ");
        assert_eq!(lexer.current_token, Token::StringConstant("foo"));
    }

    #[test]
    fn parse_integer() {
        let lexer = JackTokenizer::new(" 0 ");
        assert_eq!(lexer.current_token, Token::IntegerConstant(0));
    }

    #[test]
    fn parse_symbol() {
        let lexer = JackTokenizer::new("+");
        assert_eq!(lexer.current_token, Token::Symbol('+'));
    }

    #[test]
    fn parse_keyword() {
        let lexer = JackTokenizer::new(" class ");
        assert_eq!(lexer.current_token, Token::Keyword(Keyword::Class));
    }

    #[test]
    fn tokens_separate_by_symbol() {
        let lexer = JackTokenizer::new("x+1");
        assert_eq!(lexer.current_token, Token::Identifier("x"));
        let lexer = lexer.advance();
        assert_eq!(lexer.current_token, Token::Symbol('+'));
        let lexer = lexer.advance();
        assert_eq!(lexer.current_token, Token::IntegerConstant(1));
    }

    #[test]
    fn identifier_can_contain_numbers() {
        let lexer = JackTokenizer::new("x1");
        assert_eq!(lexer.current_token, Token::Identifier("x1"));
    }
}
