use crate::chapter10_compiler::tokenizer::{JackTokenizer, Keyword as KW, Keyword, Token};
use minifb::Key::K;
use std::ops::BitOr;
use std::process::Output;

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
    UnaryOp(char, Box<ParseTree>),
    BinaryOp(char, Box<ParseTree>, Box<ParseTree>),
    ArrayIndex(String, Box<ParseTree>),
    FunctionCall(String, Vec<ParseTree>),
    MethodCall(String, String, Vec<ParseTree>),
    Let(String, Option<Box<ParseTree>>, Box<ParseTree>),
}

impl ParseTree {
    fn string(s: impl ToString) -> Self {
        ParseTree::String(s.to_string())
    }

    fn var_name(s: impl ToString) -> Self {
        ParseTree::VarName(s.to_string())
    }

    fn not(pt: ParseTree) -> Self {
        ParseTree::UnaryOp('~', Box::new(pt))
    }

    fn neg(pt: ParseTree) -> Self {
        ParseTree::UnaryOp('-', Box::new(pt))
    }

    fn array_index(arr: impl ToString, idx: ParseTree) -> Self {
        ParseTree::ArrayIndex(arr.to_string(), Box::new(idx))
    }

    fn unary_op(op: char, x: ParseTree) -> Self {
        ParseTree::UnaryOp(op, Box::new(x))
    }

    fn binary_op(op: char, a: ParseTree, b: ParseTree) -> Self {
        ParseTree::BinaryOp(op, Box::new(a), Box::new(b))
    }

    fn function_call(func: impl ToString, args: Vec<ParseTree>) -> Self {
        ParseTree::FunctionCall(func.to_string(), args)
    }

    fn method_call(obj: impl ToString, func: impl ToString, args: Vec<ParseTree>) -> Self {
        ParseTree::MethodCall(obj.to_string(), func.to_string(), args)
    }

    fn let_(varname: impl ToString, array_index: Option<ParseTree>, value: ParseTree) -> Self {
        ParseTree::Let(
            varname.to_string(),
            array_index.map(Box::new),
            Box::new(value),
        )
    }
}

trait Parser {
    type Output;
    fn parse<'s>(&self, lexer: JackTokenizer<'s>) -> ParseResult<'s, Self::Output>;
}

impl Parser for char {
    type Output = char;
    fn parse<'s>(&self, lexer: JackTokenizer<'s>) -> ParseResult<'s, Self::Output> {
        match lexer.current_token {
            Token::Symbol(ch) if ch == *self => Ok((ch, lexer.advance())),
            _ => Err(lexer),
        }
    }
}

impl Parser for Keyword {
    type Output = Keyword;
    fn parse<'s>(&self, lexer: JackTokenizer<'s>) -> ParseResult<'s, Self::Output> {
        match lexer.current_token {
            Token::Keyword(kw) if kw == *self => Ok((kw, lexer.advance())),
            _ => Err(lexer),
        }
    }
}

impl Parser for &'static str {
    type Output = Keyword;
    fn parse<'s>(&self, lexer: JackTokenizer<'s>) -> ParseResult<'s, Self::Output> {
        match lexer.current_token {
            Token::Keyword(kw) if kw == Keyword::try_from_str(self).unwrap() => {
                Ok((kw, lexer.advance()))
            }
            _ => Err(lexer),
        }
    }
}

struct ParserWrapper<T: Parser>(T);

impl<T: Parser> Parser for ParserWrapper<T> {
    type Output = T::Output;
    fn parse<'s>(&self, lexer: JackTokenizer<'s>) -> ParseResult<'s, Self::Output> {
        self.0.parse(lexer)
    }
}

struct Either<A, B>(A, B);

impl<A, B> Parser for Either<A, B>
where
    A: Parser,
    B: Parser<Output = A::Output>,
{
    type Output = B::Output;
    fn parse<'s>(&self, lexer: JackTokenizer<'s>) -> ParseResult<'s, Self::Output> {
        match self.0.parse(lexer) {
            Err(_) => {}
            ok => return ok,
        }
        self.1.parse(lexer)
    }
}

macro_rules! parser {
    ($lhs:ident: $t:ty = $($rhs:tt)+) => {
        fn $lhs(lexer: JackTokenizer) -> ParseResult<$t> {
            parser!(@parse lexer, $($rhs)+)
        }
    };

    ($lhs:ident = $($rhs:tt)+) => {
        fn $lhs(lexer: JackTokenizer) -> ParseResult {
            parser!(@parse lexer, $($rhs)+)
        }
    };

    (@parse $lexer:expr, ( $p:tt ) ) => {
        parser!(@parse $lexer, $p)
    };

    (@parse $lexer:expr, (* $($p:tt)+) ) => {{
        let mut lexer = $lexer;
        let mut items = vec![];
        loop {
            match parser!(@parse lexer, ($($p)+)) {
                Ok((val, lx)) => {
                    items.push(val);
                    lexer = lx;
                }
                Err(_) => break,
            }
        }
        let result: Result<_, JackTokenizer> = Ok((items, lexer));
        result
    }};

    (@parse $lexer:expr, (=> $res:expr) ) => {
        Ok(($res, $lexer))
    };

    (@parse $lexer:expr, ( $var:ident @ $first:tt $($rest:tt)* ) ) => {
        match parser!(@parse $lexer, $first) {
            Ok((val, lx)) => { let $var = val; parser!(@parse lx, ( $($rest)* ) ) }
            Err(_) => Err($lexer),
        }
    };

    (@parse $lexer:expr, ( $first:tt $($rest:tt)* ) ) => {
        match parser!(@parse $lexer, $first) {
            Ok((_, lx)) => { parser!(@parse lx, ( $($rest)* ) ) }
            Err(_) => Err($lexer),
        }
    };

    (@parse $lexer:expr, $maybe:tt | $($rest:tt)*) => {
        match parser!(@parse $lexer, $maybe) {
            Err(_) => { parser!(@parse $lexer, $($rest)*) }
            ok => ok,
        }
    };

    (@parse $lexer:expr, $p:ident) => {
        $p($lexer)
    };

    (@parse $lexer:expr, $literal:expr) => {
        Parser::parse(&$literal, $lexer)
    };
}

parser! {
    parse_statement = parse_let_statement
}

parser! {
    //parse_let_statement = "let" (var @ parse_var_name) '=' (val @ parse_expression) ';' => ParseTree::let_(var, None, val)
    parse_let_statement = ("let" var @ identifier '=' val @ expression ';' => ParseTree::let_(var, None, val))
}

fn parse_a(lexer: JackTokenizer) -> ParseResult {
    unimplemented!()
}
fn parse_b(lexer: JackTokenizer) -> ParseResult {
    unimplemented!()
}

fn _parse_let_statement(lexer: JackTokenizer) -> ParseResult {
    let original_lexer = lexer;

    let mut lexer = consume_keyword(lexer, Keyword::Let)?;

    let varname = if let Token::Identifier(name) = lexer.current_token {
        lexer = lexer.advance();
        name.to_string()
    } else {
        return Err(original_lexer);
    };

    let array_index = match lexer.current_token {
        Token::Symbol('[') => {
            let lx = consume_symbol(lexer, '[')?;
            let (index, lx) = expression(lexer)?;
            let lx = consume_symbol(lexer, ']')?;
            lexer = lx;
            Some(index)
        }
        _ => None,
    };

    let lexer = consume_symbol(lexer, '=')?;

    let (value, lexer) = expression(lexer)?;

    let lexer = consume_symbol(lexer, ';')?;

    Ok((ParseTree::let_(varname, array_index, value), lexer))
}

fn expression(lexer: JackTokenizer) -> ParseResult {
    let (mut val, mut lexer) = term(lexer)?;
    loop {
        match lexer.current_token {
            Token::Symbol(op @ ('+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '=')) => {
                let (rhs, lx) = term(lexer.advance())?;
                val = ParseTree::binary_op(op, val, rhs);
                lexer = lx;
            }
            _ => return Ok((val, lexer)),
        }
    }
}

parser! {
    term = integer_constant
         | string_constant
         | keyword_constant
         | (var @ identifier '[' idx @ expression ']' => ParseTree::array_index(var, idx))
         | (obj @ identifier '.' fun @ identifier args @ expression_list => ParseTree::method_call(obj, fun, args))
         | (fun @ identifier args @ expression_list => ParseTree::function_call(fun, args))
         | (var @ identifier => ParseTree::var_name(var))
         | ('(' x @ expression ')' => x)
         | (op @ parse_unary_op val @ term => ParseTree::unary_op(op, val))
}

parser! {
    keyword_constant = ("true" => ParseTree::True)
                     | ("false" => ParseTree::False)
                     | ("null" => ParseTree::Null)
                     | ("this" => ParseTree::This)
}

parser! {
    parse_unary_op: char = '-' | '~'
}

fn expression_list(mut lexer: JackTokenizer) -> ParseResult<Vec<ParseTree>> {
    lexer = consume_symbol(lexer, '(')?;
    let mut items = vec![];
    loop {
        match lexer.current_token {
            Token::Symbol(')') => break,
            Token::Symbol(',') => lexer = consume_symbol(lexer, ',')?,
            _ => {
                let (item, lx) = expression(lexer)?;
                items.push(item);
                lexer = lx;
            }
        }
    }
    lexer = consume_symbol(lexer, ')')?;
    Ok((items, lexer))
}

fn identifier<'s>(lexer: JackTokenizer<'s>) -> ParseResult<&'s str> {
    match lexer.current_token {
        Token::Identifier(name) => Ok((name, lexer.advance())),
        _ => Err(lexer),
    }
}

fn integer_constant(lexer: JackTokenizer) -> ParseResult {
    match lexer.current_token {
        Token::IntegerConstant(x) => Ok((ParseTree::Integer(x), lexer.advance())),
        _ => Err(lexer),
    }
}

fn string_constant(lexer: JackTokenizer) -> ParseResult {
    match lexer.current_token {
        Token::StringConstant(x) => Ok((ParseTree::String(x.to_string()), lexer.advance())),
        _ => Err(lexer),
    }
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

fn consume_keyword(
    lexer: JackTokenizer,
    expected: Keyword,
) -> Result<JackTokenizer, JackTokenizer> {
    if let Token::Keyword(kw) = lexer.current_token {
        if kw == expected {
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
            term(JackTokenizer::new("42")),
            Ok((ParseTree::Integer(42), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_string() {
        assert_eq!(
            term(JackTokenizer::new("\"0\"")),
            Ok((ParseTree::string("0"), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_keyword_constants() {
        assert_eq!(
            term(JackTokenizer::new("true")),
            Ok((ParseTree::True, JackTokenizer::end()))
        );
        assert_eq!(
            term(JackTokenizer::new("false")),
            Ok((ParseTree::False, JackTokenizer::end()))
        );
        assert_eq!(
            term(JackTokenizer::new("null")),
            Ok((ParseTree::Null, JackTokenizer::end()))
        );
        assert_eq!(
            term(JackTokenizer::new("this")),
            Ok((ParseTree::This, JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_variable() {
        assert_eq!(
            term(JackTokenizer::new("xyz")),
            Ok((ParseTree::var_name("xyz"), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_array_index() {
        assert_eq!(
            term(JackTokenizer::new("x[a]")),
            Ok((
                ParseTree::array_index("x", ParseTree::var_name("a")),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_term_in_parentheses() {
        assert_eq!(
            term(JackTokenizer::new("(foo)")),
            term(JackTokenizer::new("foo"))
        );
    }

    #[test]
    fn parse_term_unary() {
        assert_eq!(
            term(JackTokenizer::new("~true")),
            Ok((ParseTree::not(ParseTree::True), JackTokenizer::end()))
        );
        assert_eq!(
            term(JackTokenizer::new("-1")),
            Ok((ParseTree::neg(ParseTree::Integer(1)), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_subroutine_call() {
        assert_eq!(
            term(JackTokenizer::new("foo()")),
            Ok((
                ParseTree::FunctionCall("foo".to_string(), vec![]),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            term(JackTokenizer::new("foo.bar()")),
            Ok((
                ParseTree::MethodCall("foo".to_string(), "bar".to_string(), vec![]),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_expr_list() {
        assert_eq!(
            expression_list(JackTokenizer::new("()")),
            Ok((vec![], JackTokenizer::end()))
        );
        assert_eq!(
            expression_list(JackTokenizer::new("(1)")),
            Ok((vec![ParseTree::Integer(1)], JackTokenizer::end()))
        );
        assert_eq!(
            expression_list(JackTokenizer::new("(1,2)")),
            Ok((
                vec![ParseTree::Integer(1), ParseTree::Integer(2)],
                JackTokenizer::end()
            ))
        );
        assert_eq!(
            expression_list(JackTokenizer::new("(,)")),
            Ok((vec![], JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_unary_expression() {
        assert_eq!(
            expression(JackTokenizer::new("42")),
            Ok((ParseTree::Integer(42), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_binary_expression() {
        assert_eq!(
            expression(JackTokenizer::new("1 + 2")),
            Ok((
                ParseTree::binary_op('+', ParseTree::Integer(1), ParseTree::Integer(2)),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_operators_ignore_precedence() {
        assert_eq!(
            expression(JackTokenizer::new("2 + 3 * 4")),
            Ok((
                ParseTree::binary_op(
                    '*',
                    ParseTree::binary_op('+', ParseTree::Integer(2), ParseTree::Integer(3)),
                    ParseTree::Integer(4)
                ),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_parentheses_make_precedence() {
        assert_eq!(
            expression(JackTokenizer::new("(2 + 3) * (4 - 5)")),
            Ok((
                ParseTree::binary_op(
                    '*',
                    ParseTree::binary_op('+', ParseTree::Integer(2), ParseTree::Integer(3)),
                    ParseTree::binary_op('-', ParseTree::Integer(4), ParseTree::Integer(5)),
                ),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_let_statement() {
        assert_eq!(
            parse_statement(JackTokenizer::new("let foo = bar;")),
            Ok((
                ParseTree::let_("foo", None, ParseTree::var_name("bar")),
                JackTokenizer::end()
            ))
        );
    }
}
