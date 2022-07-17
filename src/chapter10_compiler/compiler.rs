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
    ArrayIndex(String, Box<ParseTree>),
    FunctionCall(String, Vec<ParseTree>),
    MethodCall(String, String, Vec<ParseTree>),
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
    let token = lexer.current_token;
    if lexer.has_more_tokens() {
        *lexer = lexer.advance();
    }
    match token {
        Token::IntegerConstant(x) => Some(ParseTree::Integer(x)),
        Token::StringConstant(x) => Some(ParseTree::String(x.to_string())),
        Token::Keyword(True) => Some(ParseTree::True),
        Token::Keyword(False) => Some(ParseTree::False),
        Token::Keyword(Null) => Some(ParseTree::Null),
        Token::Keyword(This) => Some(ParseTree::This),
        Token::Identifier(name) => match lexer.current_token {
            Token::Symbol('[') => {
                consume_symbol(lexer, '[')?;
                let index = parse_expression(lexer)?;
                consume_symbol(lexer, ']')?;
                Some(ParseTree::array_index(name, index))
            }
            Token::Symbol('(') => {
                let args = parse_expression_list(lexer)?;
                Some(ParseTree::FunctionCall(name.to_string(), args))
            }
            Token::Symbol('.') => {
                consume_symbol(lexer, '.')?;
                let method = parse_identifier(lexer)?;
                let args = parse_expression_list(lexer)?;
                Some(ParseTree::MethodCall(name.to_string(), method.to_string(), args))
            }
            _ => Some(ParseTree::var_name(name)),
        },
        Token::Symbol('(') => {
            let expr = parse_expression(lexer)?;
            consume_symbol(lexer, ')')?;
            Some(expr)
        }
        Token::Symbol('-') => Some(ParseTree::neg(parse_term(lexer).unwrap())),
        Token::Symbol('~') => Some(ParseTree::not(parse_term(lexer).unwrap())),
        _ => None,
    }
}

fn parse_expression_list(lexer: &mut JackTokenizer) -> Option<Vec<ParseTree>> {
    consume_symbol(lexer, '(')?;
    let mut items = vec![];
    loop {
        match lexer.current_token {
            Token::Symbol(')') => break,
            Token::Symbol(',') => consume_symbol(lexer, ',')?,
            _ => {
                items.push(parse_expression(lexer)?);
            }
        }
    }
    consume_symbol(lexer, ')')?;
    Some(items)
}

fn parse_identifier<'s>(lexer: &mut JackTokenizer<'s>) -> Option<&'s str> {
    match lexer.current_token {
        Token::Identifier(name) => {
            *lexer = lexer.advance();
            Some(name)
        }
        _ => None
    }
}

fn consume_symbol(lexer: &mut JackTokenizer, expected_symbol: char) -> Option<()> {
    if let Token::Symbol(name) = lexer.current_token {
        if name == expected_symbol {
            if lexer.has_more_tokens() {
                *lexer = lexer.advance();
            }

            return Some(());
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_term_integer() {
        assert_eq!(
            parse_term(&mut JackTokenizer::new("42").advance()),
            Some(ParseTree::Integer(42))
        );
    }

    #[test]
    fn parse_term_string() {
        assert_eq!(
            parse_term(&mut JackTokenizer::new("\"0\"").advance()),
            Some(ParseTree::string("0"))
        );
    }

    #[test]
    fn parse_term_keyword_constants() {
        assert_eq!(
            parse_term(&mut JackTokenizer::new("true").advance()),
            Some(ParseTree::True)
        );
        assert_eq!(
            parse_term(&mut JackTokenizer::new("false").advance()),
            Some(ParseTree::False)
        );
        assert_eq!(
            parse_term(&mut JackTokenizer::new("null").advance()),
            Some(ParseTree::Null)
        );
        assert_eq!(
            parse_term(&mut JackTokenizer::new("this").advance()),
            Some(ParseTree::This)
        );
    }

    #[test]
    fn parse_term_variable() {
        assert_eq!(
            parse_term(&mut JackTokenizer::new("xyz").advance()),
            Some(ParseTree::var_name("xyz"))
        );
    }

    #[test]
    fn parse_term_array_index() {
        assert_eq!(
            parse_term(&mut JackTokenizer::new("x[a]").advance()),
            Some(ParseTree::array_index("x", ParseTree::var_name("a")))
        );
    }

    #[test]
    fn parse_term_in_parentheses() {
        assert_eq!(
            parse_term(&mut JackTokenizer::new("(foo)").advance()),
            parse_term(&mut JackTokenizer::new("foo").advance())
        );
    }

    #[test]
    fn parse_term_unary() {
        assert_eq!(
            parse_term(&mut JackTokenizer::new("~true").advance()),
            Some(ParseTree::not(ParseTree::True))
        );
        assert_eq!(
            parse_term(&mut JackTokenizer::new("-1").advance()),
            Some(ParseTree::neg(ParseTree::Integer(1)))
        );
    }

    #[test]
    fn parse_term_subroutine_call() {
        assert_eq!(
            parse_term(&mut JackTokenizer::new("foo()").advance()),
            Some(ParseTree::FunctionCall("foo".to_string(), vec![]))
        );

        assert_eq!(
            parse_term(&mut JackTokenizer::new("foo.bar()").advance()),
            Some(ParseTree::MethodCall(
                "foo".to_string(),
                "bar".to_string(),
                vec![]
            ))
        );
    }

    #[test]
    fn parse_expr_list() {
        assert_eq!(
            parse_expression_list(&mut JackTokenizer::new("()").advance()),
            Some(vec![])
        );
        assert_eq!(
            parse_expression_list(&mut JackTokenizer::new("(1)").advance()),
            Some(vec![ParseTree::Integer(1)])
        );
        assert_eq!(
            parse_expression_list(&mut JackTokenizer::new("(1,2)").advance()),
            Some(vec![ParseTree::Integer(1), ParseTree::Integer(2)])
        );
        assert_eq!(
            parse_expression_list(&mut JackTokenizer::new("(,)").advance()),
            Some(vec![])
        );
    }
}
