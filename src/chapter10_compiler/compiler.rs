use crate::chapter10_compiler::tokenizer::{JackTokenizer, Keyword as KW, Token};

#[derive(Debug, Eq, PartialEq)]
enum ParseTree {
    True,
    False,
    Null,
    This,
    Integer(u16),
    String(String),
    VarName(String),
    Not(Box<ParseTree>),
    Neg(Box<ParseTree>),
    ArrayIndex(String, Box<ParseTree>)
}

impl ParseTree {
    fn string(s: impl ToString) -> Self {
        ParseTree::String(s.to_string())
    }

    fn var_name(s: impl ToString) -> Self {
        ParseTree::VarName(s.to_string())
    }

    fn not(pt: ParseTree) -> Self {
        ParseTree::Not(Box::new(pt))
    }

    fn neg(pt: ParseTree) -> Self {
        ParseTree::Neg(Box::new(pt))
    }

    fn array_index(arr: impl ToString, idx: ParseTree) -> Self {
        ParseTree::ArrayIndex(arr.to_string(), Box::new(idx))
    }
}

fn parse_expression(lexer: &mut JackTokenizer) -> Option<ParseTree> {
    parse_term(lexer)
}

fn parse_term(lexer: &mut JackTokenizer) -> Option<ParseTree> {
    use KW::*;
    let
    match lexer.current_token {
        Token::IntegerConstant(x) => Some(ParseTree::Integer(x)),
        Token::StringConstant(x) => Some(ParseTree::String(x.to_string())),
        Token::Keyword(True) => Some(ParseTree::True),
        Token::Keyword(False) => Some(ParseTree::False),
        Token::Keyword(Null) => Some(ParseTree::Null),
        Token::Keyword(This) => Some(ParseTree::This),
        Token::Identifier(name) => Some(ParseTree::var_name(name)),
        Token::Symbol('-') => {
            *lexer = lexer.advance();
            Some(ParseTree::neg(parse_term(lexer).unwrap()))
        }
        Token::Symbol('~') => {
            *lexer = lexer.advance();
            Some(ParseTree::not(parse_term(lexer).unwrap()))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_expression_integer() {
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("42").advance()),
            Some(ParseTree::Integer(42))
        );
    }

    #[test]
    fn parse_expression_string() {
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("\"0\"").advance()),
            Some(ParseTree::string("0"))
        );
    }

    #[test]
    fn parse_expression_variable() {
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("xyz").advance()),
            Some(ParseTree::var_name("xyz"))
        );
    }

    #[test]
    fn parse_expression_array_index() {
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("x[a]").advance()),
            Some(ParseTree::array_index("x", ParseTree::var_name("a")))
        );
    }

    #[test]
    fn parse_expression_keyword_constants() {
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("true").advance()),
            Some(ParseTree::True)
        );
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("false").advance()),
            Some(ParseTree::False)
        );
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("null").advance()),
            Some(ParseTree::Null)
        );
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("this").advance()),
            Some(ParseTree::This)
        );
    }

    #[test]
    fn parse_expression_unary() {
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("~true").advance()),
            Some(ParseTree::not(ParseTree::True))
        );
        assert_eq!(
            parse_expression(&mut JackTokenizer::new("-1").advance()),
            Some(ParseTree::neg(ParseTree::Integer(1)))
        );
    }
}
