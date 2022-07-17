use crate::chapter10_compiler::tokenizer::{JackTokenizer, Keyword as KW, Token};
use std::ops::BitOr;

type ParseResult<'s, T = ParseTree> = Result<(T, JackTokenizer<'s>), JackTokenizer<'s>>;

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

macro_rules! parser {
    ($lhs:ident = $rhs:ident) => {
        fn $lhs(lexer: JackTokenizer) -> ParseResult {
            $rhs(lexer)
        }
    };

    ($lhs:ident = $($options:ident)|*) => {
        fn $lhs(lexer: JackTokenizer) -> ParseResult {
            $(
                let lexer = match $options(lexer) {
                    Err(lx) => lx,
                    ok => return ok,
                };
            )*
            Err(lexer)
        }
    };
}

fn parse_expression(lexer: JackTokenizer) -> ParseResult {
    parse_term(lexer)
}

parser! {
    parse_term = parse_integer_constant
               | parse_string_constant
               | parse_keyword_constant
               | parse_identifier_term
               | parse_parenthesized_expression
               | parse_unary_op
}

fn parse_parenthesized_expression(lexer: JackTokenizer) -> ParseResult {
    let lexer = consume_symbol(lexer, '(')?;
    let (expr, lexer) = parse_expression(lexer)?;
    let lexer = consume_symbol(lexer, ')')?;
    Ok((expr, lexer))
}

fn parse_expression_list(mut lexer: JackTokenizer) -> ParseResult<Vec<ParseTree>> {
    lexer = consume_symbol(lexer, '(')?;
    let mut items = vec![];
    loop {
        match lexer.current_token {
            Token::Symbol(')') => break,
            Token::Symbol(',') => lexer = consume_symbol(lexer, ',')?,
            _ => {
                let (item, lx) = parse_expression(lexer)?;
                items.push(item);
                lexer = lx;
            }
        }
    }
    lexer = consume_symbol(lexer, ')')?;
    Ok((items, lexer))
}

fn parse_identifier<'s>(lexer: JackTokenizer<'s>) -> ParseResult<&'s str> {
    match lexer.current_token {
        Token::Identifier(name) => Ok((name, lexer.advance())),
        _ => Err(lexer),
    }
}

fn parse_integer_constant(lexer: JackTokenizer) -> ParseResult {
    match lexer.current_token {
        Token::IntegerConstant(x) => Ok((ParseTree::Integer(x), lexer.advance())),
        _ => Err(lexer),
    }
}

fn parse_string_constant(lexer: JackTokenizer) -> ParseResult {
    match lexer.current_token {
        Token::StringConstant(x) => Ok((ParseTree::String(x.to_string()), lexer.advance())),
        _ => Err(lexer),
    }
}

fn parse_keyword_constant(lexer: JackTokenizer) -> ParseResult {
    use KW::*;
    let parsed = match lexer.current_token {
        Token::Keyword(True) => Some(ParseTree::True),
        Token::Keyword(False) => Some(ParseTree::False),
        Token::Keyword(Null) => Some(ParseTree::Null),
        Token::Keyword(This) => Some(ParseTree::This),
        _ => None,
    };
    parsed.map(|pt| (pt, lexer.advance())).ok_or(lexer)
}

fn parse_identifier_term(mut lexer: JackTokenizer) -> ParseResult {
    let original_lexer = lexer;

    let varname = if let Token::Identifier(name) = lexer.current_token {
        lexer = lexer.advance();
        name.to_string()
    } else {
        return Err(original_lexer);
    };

    match lexer.current_token {
        Token::Symbol('[') => {
            let lexer = consume_symbol(lexer, '[')?;
            let (index, lexer) = parse_expression(lexer)?;
            let lexer = consume_symbol(lexer, ']')?;
            Ok((ParseTree::array_index(varname, index), lexer))
        }
        Token::Symbol('(') => {
            let (args, lexer) = parse_expression_list(lexer)?;
            Ok((ParseTree::FunctionCall(varname, args), lexer))
        }
        Token::Symbol('.') => {
            let lexer = consume_symbol(lexer, '.')?;
            let (method, lexer) = parse_identifier(lexer)?;
            let (args, lexer) = parse_expression_list(lexer)?;
            Ok((
                ParseTree::MethodCall(varname, method.to_string(), args),
                lexer,
            ))
        }
        _ => Ok((ParseTree::VarName(varname), lexer)),
    }
}

fn parse_unary_op(mut lexer: JackTokenizer) -> ParseResult {
    let op_cons = match lexer.current_token {
        Token::Symbol('-') => ParseTree::neg,
        Token::Symbol('~') => ParseTree::not,
        _ => return Err(lexer),
    };

    let (operand, lx) = parse_term(lexer.advance())?;
    Ok((op_cons(operand), lx))
}

fn consume_symbol(
    lexer: JackTokenizer,
    expected_symbol: char,
) -> Result<JackTokenizer, JackTokenizer> {
    if let Token::Symbol(name) = lexer.current_token {
        if name == expected_symbol {
            return Ok(lexer.advance());
        }
    }
    Err(lexer)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_term_integer() {
        assert_eq!(
            parse_term(JackTokenizer::new("42")),
            Ok((ParseTree::Integer(42), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_string() {
        assert_eq!(
            parse_term(JackTokenizer::new("\"0\"")),
            Ok((ParseTree::string("0"), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_keyword_constants() {
        assert_eq!(
            parse_term(JackTokenizer::new("true")),
            Ok((ParseTree::True, JackTokenizer::end()))
        );
        assert_eq!(
            parse_term(JackTokenizer::new("false")),
            Ok((ParseTree::False, JackTokenizer::end()))
        );
        assert_eq!(
            parse_term(JackTokenizer::new("null")),
            Ok((ParseTree::Null, JackTokenizer::end()))
        );
        assert_eq!(
            parse_term(JackTokenizer::new("this")),
            Ok((ParseTree::This, JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_variable() {
        assert_eq!(
            parse_term(JackTokenizer::new("xyz")),
            Ok((ParseTree::var_name("xyz"), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_array_index() {
        assert_eq!(
            parse_term(JackTokenizer::new("x[a]")),
            Ok((
                ParseTree::array_index("x", ParseTree::var_name("a")),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_term_in_parentheses() {
        assert_eq!(
            parse_term(JackTokenizer::new("(foo)")),
            parse_term(JackTokenizer::new("foo"))
        );
    }

    #[test]
    fn parse_term_unary() {
        assert_eq!(
            parse_term(JackTokenizer::new("~true")),
            Ok((ParseTree::not(ParseTree::True), JackTokenizer::end()))
        );
        assert_eq!(
            parse_term(JackTokenizer::new("-1")),
            Ok((ParseTree::neg(ParseTree::Integer(1)), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_subroutine_call() {
        assert_eq!(
            parse_term(JackTokenizer::new("foo()")),
            Ok((
                ParseTree::FunctionCall("foo".to_string(), vec![]),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            parse_term(JackTokenizer::new("foo.bar()")),
            Ok((
                ParseTree::MethodCall("foo".to_string(), "bar".to_string(), vec![]),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_expr_list() {
        assert_eq!(
            parse_expression_list(JackTokenizer::new("()")),
            Ok((vec![], JackTokenizer::end()))
        );
        assert_eq!(
            parse_expression_list(JackTokenizer::new("(1)")),
            Ok((vec![ParseTree::Integer(1)], JackTokenizer::end()))
        );
        assert_eq!(
            parse_expression_list(JackTokenizer::new("(1,2)")),
            Ok((
                vec![ParseTree::Integer(1), ParseTree::Integer(2)],
                JackTokenizer::end()
            ))
        );
        assert_eq!(
            parse_expression_list(JackTokenizer::new("(,)")),
            Ok((vec![], JackTokenizer::end()))
        );
    }
}
