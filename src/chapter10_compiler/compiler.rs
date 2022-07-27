use crate::chapter10_compiler::tokenizer::{JackTokenizer, Keyword as KW, Keyword, Token};
use minifb::Key::K;
use std::ops::BitOr;
use std::process::Output;

type ParseResult<'s, T> = Result<(T, JackTokenizer<'s>), JackTokenizer<'s>>;

#[derive(Debug, Eq, PartialEq)]
enum ClassVarDec {
    Static(Type, Vec<String>),
    Field(Type, Vec<String>),
}

#[derive(Debug, Eq, PartialEq)]
struct SubroutineDec(
    SubroutineKind,
    Type,
    String,
    Vec<(Type, String)>,
    SubroutineBody,
);

#[derive(Debug, Eq, PartialEq)]
enum SubroutineKind {
    Function,
    Constructor,
    Method,
}

#[derive(Debug, Eq, PartialEq)]
struct SubroutineBody(Vec<VarDec>, Vec<Statement>);

impl SubroutineBody {
    fn new(vars: Vec<VarDec>, stmts: Vec<Statement>) -> Self {
        SubroutineBody(vars, stmts)
    }
}

#[derive(Debug, Eq, PartialEq)]
struct VarDec(Type, Vec<String>);

impl VarDec {
    fn new(typ: Type, vars: Vec<&str>) -> Self {
        VarDec(typ, vars.into_iter().map(str::to_string).collect())
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Type {
    Void,
    Int,
    Char,
    Bool,
    Class(String),
}

#[derive(Debug, Eq, PartialEq)]
enum Statement {
    Let(String, Option<Expression>, Expression),
    If(Expression, Vec<Statement>, Vec<Statement>),
    While(Expression, Vec<Statement>),
    Do(SubroutineCall),
    Return(Option<Expression>),
}

impl Statement {
    fn let_(
        varname: impl ToString,
        array_index: Option<Expression>,
        value: impl Into<Expression>,
    ) -> Self {
        Self::Let(varname.to_string(), array_index, value.into())
    }

    fn if_(
        condition: impl Into<Expression>,
        consequence: Vec<Statement>,
        alternative: Vec<Statement>,
    ) -> Self {
        Self::If(condition.into(), consequence, alternative)
    }

    fn while_(condition: impl Into<Expression>, body: Vec<Statement>) -> Self {
        Self::While(condition.into(), body)
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Expression {
    Term(Term),
    Op(Term, char, Box<Expression>),
}

impl Expression {
    fn integer(x: u16) -> Self {
        Self::Term(Term::integer(x))
    }

    fn term(x: impl Into<Term>) -> Self {
        match x.into() {
            Term::Expression(x) => *x,
            t => Expression::Term(t),
        }
    }

    fn op(op: char, a: impl Into<Term>, b: impl Into<Box<Expression>>) -> Self {
        Self::Op(a.into(), op, b.into())
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Term {
    Null,
    True,
    False,
    This,
    Integer(u16),
    String(String),
    Variable(String),
    ArrayIndex(String, Box<Expression>),
    Expression(Box<Expression>),
    Neg(Box<Term>),
    Not(Box<Term>),
    Call(SubroutineCall),
}

impl Term {
    fn integer(x: u16) -> Self {
        Self::Integer(x)
    }

    fn string(s: impl ToString) -> Self {
        Self::String(s.to_string())
    }

    fn variable(name: impl ToString) -> Self {
        Self::Variable(name.to_string())
    }

    fn array_index(name: impl ToString, idx: Expression) -> Self {
        Self::ArrayIndex(name.to_string(), Box::new(idx))
    }

    fn expression(x: impl Into<Expression>) -> Self {
        match x.into() {
            Expression::Term(t) => t,
            x => Term::Expression(Box::new(x)),
        }
    }

    fn neg(x: Term) -> Self {
        Self::Neg(Box::new(x))
    }

    fn not(x: Term) -> Self {
        Self::Not(Box::new(x))
    }
}

impl From<Term> for Box<Expression> {
    fn from(t: Term) -> Self {
        match t {
            Term::Expression(x) => x,
            _ => Box::new(Expression::Term(t)),
        }
    }
}

impl From<Term> for Expression {
    fn from(t: Term) -> Self {
        Self::term(t)
    }
}

impl From<Expression> for Term {
    fn from(x: Expression) -> Self {
        Self::expression(x)
    }
}

impl From<u16> for Expression {
    fn from(x: u16) -> Self {
        Self::integer(x)
    }
}

impl From<u16> for Term {
    fn from(x: u16) -> Self {
        Self::integer(x)
    }
}

#[derive(Debug, Eq, PartialEq)]
enum SubroutineCall {
    FunctionCall(String, Vec<Expression>),
    MethodCall(String, String, Vec<Expression>),
}

impl SubroutineCall {
    fn function_call(func: impl ToString, args: Vec<Expression>) -> Self {
        Self::FunctionCall(func.to_string(), args)
    }

    fn method_call(obj: impl ToString, func: impl ToString, args: Vec<Expression>) -> Self {
        Self::MethodCall(obj.to_string(), func.to_string(), args)
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

    (@parse $lexer:expr, (+ $($p:tt)+) ) => {
        match parser!(@parse $lexer, (* $($p)+)) {
            Ok((items, lexer)) if items.len() >= 1 => Ok((items, lexer)),
            _ => Err($lexer)
        }
    };

    (@parse $lexer:expr, (=> $res:expr) ) => {
        Ok(($res, $lexer))
    };

    (@parse $lexer:expr, ( $var:ident @ $first:tt $($rest:tt)* ) ) => {
        match parser!(@parse $lexer, $first) {
            Ok((val, lx)) => { let $var = val; parser!(@parse lx, ( $($rest)* ) ) }
            Err(_) => Err($lexer),
        }
    };

    (@parse $lexer:expr, ( ($var:ident @ $first:tt) $($rest:tt)* ) ) => {
        parser!(@parse $lexer, ($var @ $first $($rest)*))
    };

    (@parse $lexer:expr, ( $($option:tt)|* ) ) => {
        Err($lexer)
        $(
            .or_else(|_|parser!(@parse $lexer, $option))
        )*
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
    type_: Type = ("int" => Type::Int)
                | ("char" => Type::Char)
                | ("bool" => Type::Bool)
                | (cls @ identifier => Type::Class(cls.to_string()))
}

parser! {
    classvar_dec: ClassVarDec = ("static" typ @ type_ n0@var_name names@(* (',' n@var_name => n)) ';' => ClassVarDec::Static(typ, cons(n0, names)))
                              | ("field" typ @ type_ n0@var_name names@(* (',' n@var_name => n)) ';' => ClassVarDec::Field(typ, cons(n0, names)))
}

parser! {
    subroutine_dec: SubroutineDec = (kind @ (("constructor" => SubroutineKind::Constructor)
                                             | ("function" => SubroutineKind::Function)
                                             | ("method" => SubroutineKind::Method))
                                     typ @ (("void" => Type::Void) | type_)
                                     name @ subroutine_name
                                     '(' params @ parameter_list ')'
                                     body @ subroutine_body
                                     => SubroutineDec(kind, typ, name, params, body))
}

parser! {
    parameter_list : Vec<(Type, String)> = (t0@type_ n0@var_name pars @ (*(',' t@type_ n@var_name => (t, n))) => cons((t0, n0), pars))
                                         | (par @ (t@type_ n@var_name => (t,n)) => vec![par])
                                         | ( => vec![])
}

parser! {
    subroutine_body : SubroutineBody = ('{' vdcs @ (*vardec) stmts @ (*statement) '}' => SubroutineBody(vdcs, stmts))
}

parser! {
    vardec : VarDec = ("var" (typ @ type_) (fst @ var_name) (vars @ (* ',' var_name)) ';' => VarDec(typ, cons(fst, vars)))
}

parser! {
    subroutine_name : String = (name @ identifier => name.to_string())
}

parser! {
    var_name : String = (name @ identifier => name.to_string())
}

parser! {
    statements: Vec<Statement> = (* statement)
}

parser! {
    statement : Statement  = let_statement | if_statement | while_statement | do_statement | return_statement
}

parser! {
    let_statement : Statement = ("let" (var @ var_name) '=' (val @ expression) ';' => Statement::let_(var, None, val))
                              | ("let" (var @ var_name) '[' (idx @ expression) ']' '=' (val @ expression) ';' => Statement::let_(var, Some(idx), val))
}

parser! {
    if_statement : Statement = ("if" '(' cnd @ expression ')' '{' thn @ statements '}' "else" '{' alt @ statements '}' => Statement::if_(cnd, thn, alt))
                             | ("if" '(' cnd @ expression ')' '{' thn @ statements '}' => Statement::if_(cnd, thn, vec![]))
}

parser! {
    while_statement : Statement = ("while" '(' cnd @ expression ')' '{' bdy @ statements '}' => Statement::while_(cnd, bdy))
}

parser! {
    do_statement : Statement = ("do" fun @ subroutine_call ';' => Statement::Do(fun))
}

parser! {
    return_statement : Statement = ("return" ';' => Statement::Return(None))
                                 | ("return" val @ expression ';' => Statement::Return(Some(val)))
}

parser! {
    expression: Expression = (a@term op@parse_binary_op b@expression => Expression::op(op, a, Box::new(b)))
                           | (x@term => Expression::term(x))
}

parser! {
    term: Term = (x @ integer_constant => Term::Integer(x))
               | (s @ string_constant => Term::String(s))
               | keyword_constant
               | (c @ subroutine_call => Term::Call(c))
               | (var @ var_name '[' idx @ expression ']' => Term::array_index(var, idx))
               | (var @ var_name => Term::variable(var))
               | ('(' x @ expression ')' => Term::expression(x))
               | ('-' val @ term => Term::neg(val))
               | ('~' val @ term => Term::not(val))
}

parser! {
    subroutine_call : SubroutineCall = (obj @ identifier '.' fun @ identifier args @ expression_list => SubroutineCall::method_call(obj, fun, args))
                                     | (fun @ identifier args @ expression_list => SubroutineCall::function_call(fun, args))
}

parser! {
    keyword_constant : Term = ("true" => Term::True)
                            | ("false" => Term::False)
                            | ("null" => Term::Null)
                            | ("this" => Term::This)
}

parser! {
    parse_binary_op: char = '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
}

fn cons<T>(e: T, mut es: Vec<T>) -> Vec<T> {
    es.insert(0, e);
    es
}

fn expression_list(mut lexer: JackTokenizer) -> ParseResult<Vec<Expression>> {
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

fn integer_constant(lexer: JackTokenizer) -> ParseResult<u16> {
    match lexer.current_token {
        Token::IntegerConstant(x) => Ok((x, lexer.advance())),
        _ => Err(lexer),
    }
}

fn string_constant(lexer: JackTokenizer) -> ParseResult<String> {
    match lexer.current_token {
        Token::StringConstant(x) => Ok((x.to_string(), lexer.advance())),
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
            Ok((Term::Integer(42), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_string() {
        assert_eq!(
            term(JackTokenizer::new("\"0\"")),
            Ok((Term::string("0"), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_keyword_constants() {
        assert_eq!(
            term(JackTokenizer::new("true")),
            Ok((Term::True, JackTokenizer::end()))
        );
        assert_eq!(
            term(JackTokenizer::new("false")),
            Ok((Term::False, JackTokenizer::end()))
        );
        assert_eq!(
            term(JackTokenizer::new("null")),
            Ok((Term::Null, JackTokenizer::end()))
        );
        assert_eq!(
            term(JackTokenizer::new("this")),
            Ok((Term::This, JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_variable() {
        assert_eq!(
            term(JackTokenizer::new("xyz")),
            Ok((Term::variable("xyz"), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_term_array_index() {
        assert_eq!(
            term(JackTokenizer::new("x[a]")),
            Ok((
                Term::array_index("x", Expression::Term(Term::variable("a"))),
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
            Ok((Term::not(Term::True), JackTokenizer::end()))
        );
        assert_eq!(
            term(JackTokenizer::new("-1")),
            Ok((Term::neg(Term::Integer(1)), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_subroutine_call() {
        assert_eq!(
            subroutine_call(JackTokenizer::new("foo()")),
            Ok((
                SubroutineCall::FunctionCall("foo".to_string(), vec![]),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            subroutine_call(JackTokenizer::new("foo.bar()")),
            Ok((
                SubroutineCall::MethodCall("foo".to_string(), "bar".to_string(), vec![]),
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
            Ok((vec![Expression::integer(1)], JackTokenizer::end()))
        );
        assert_eq!(
            expression_list(JackTokenizer::new("(1,2)")),
            Ok((
                vec![Expression::integer(1), Expression::integer(2)],
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
            Ok((Expression::integer(42), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_binary_expression() {
        assert_eq!(
            expression(JackTokenizer::new("1 + 2")),
            Ok((
                Expression::op('+', Term::Integer(1), Expression::integer(2)),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_operators_ignore_precedence() {
        // this is the opposite of how it works in the book, where it would parse as ((2 - 3) * 4) + 5
        assert_eq!(
            expression(JackTokenizer::new("2 - 3 * 4 + 5")),
            expression(JackTokenizer::new("2 - (3 * (4 + 5))")),
        );
    }

    #[test]
    fn parse_parentheses_make_precedence() {
        assert_eq!(
            expression(JackTokenizer::new("(2 + 3) * (4 - 5)")),
            Ok((
                Expression::op(
                    '*',
                    Expression::op('+', Term::integer(2), Expression::integer(3)),
                    Expression::op('-', Term::integer(4), Expression::integer(5)),
                ),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_let_statement() {
        assert_eq!(
            statement(JackTokenizer::new("let foo = bar;")),
            Ok((
                Statement::let_("foo", None, Term::variable("bar")),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            statement(JackTokenizer::new("let foo[0] = bar;")),
            Ok((
                Statement::let_("foo", Some(Expression::integer(0)), Term::variable("bar")),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_statement_sequence() {
        assert_eq!(
            statements(JackTokenizer::new("")),
            Ok((vec![], JackTokenizer::end()))
        );

        assert_eq!(
            statements(JackTokenizer::new("let x = 0;")),
            Ok((
                vec![Statement::let_("x", None, Term::Integer(0))],
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            statements(JackTokenizer::new("let x = 0;let y=1;")),
            Ok((
                vec![
                    Statement::let_("x", None, Term::Integer(0)),
                    Statement::let_("y", None, Term::Integer(1))
                ],
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_statements_incomplete() {
        assert_eq!(
            statements(JackTokenizer::new("let x=0; let y")),
            Ok((
                vec![Statement::let_("x", None, Term::Integer(0))],
                JackTokenizer::new("let y")
            ))
        );
    }

    #[test]
    fn parse_if_statement() {
        assert_eq!(
            statement(JackTokenizer::new("if (true) {} else {}")),
            Ok((
                Statement::if_(Term::True, vec![], vec![]),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            statement(JackTokenizer::new("if (true) {}")),
            Ok((
                Statement::if_(Term::True, vec![], vec![]),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_while_statement() {
        assert_eq!(
            statement(JackTokenizer::new("while (true) {}")),
            Ok((Statement::while_(Term::True, vec![],), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_do_statement() {
        assert_eq!(
            statement(JackTokenizer::new("do foo();")),
            Ok((
                Statement::Do(SubroutineCall::function_call("foo", vec![])),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_return_statement() {
        assert_eq!(
            statement(JackTokenizer::new("return;")),
            Ok((Statement::Return(None), JackTokenizer::end()))
        );

        assert_eq!(
            statement(JackTokenizer::new("return null;")),
            Ok((
                Statement::Return(Some(Term::Null.into())),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_type() {
        assert_eq!(
            type_(JackTokenizer::new("int")),
            Ok((Type::Int, JackTokenizer::end()))
        );

        assert_eq!(
            type_(JackTokenizer::new("char")),
            Ok((Type::Char, JackTokenizer::end()))
        );

        assert_eq!(
            type_(JackTokenizer::new("bool")),
            Ok((Type::Bool, JackTokenizer::end()))
        );

        assert_eq!(
            type_(JackTokenizer::new("Custom")),
            Ok((Type::Class("Custom".to_string()), JackTokenizer::end()))
        );
    }

    #[test]
    fn parse_vardec() {
        assert_eq!(
            vardec(JackTokenizer::new("var int x;")),
            Ok((VarDec::new(Type::Int, vec!["x"]), JackTokenizer::end()))
        );

        assert_eq!(
            vardec(JackTokenizer::new("var int x, y, z;")),
            Ok((
                VarDec::new(Type::Int, vec!["x", "y", "z"]),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_subroutine_body() {
        assert_eq!(
            subroutine_body(JackTokenizer::new("{}")),
            Ok((SubroutineBody::new(vec![], vec![]), JackTokenizer::end()))
        );

        assert_eq!(
            subroutine_body(JackTokenizer::new("{return;}")),
            Ok((
                SubroutineBody::new(vec![], vec![Statement::Return(None)]),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            subroutine_body(JackTokenizer::new("{let x=0; return;}")),
            Ok((
                SubroutineBody::new(
                    vec![],
                    vec![Statement::let_("x", None, 0), Statement::Return(None)]
                ),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            subroutine_body(JackTokenizer::new("{var int x;}")),
            Ok((
                SubroutineBody::new(vec![VarDec::new(Type::Int, vec!["x"])], vec![]),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_parameter_list() {
        assert_eq!(
            parameter_list(JackTokenizer::new("")),
            Ok((vec![], JackTokenizer::end()))
        );

        assert_eq!(
            parameter_list(JackTokenizer::new("int x")),
            Ok((vec![(Type::Int, "x".to_string())], JackTokenizer::end()))
        );

        assert_eq!(
            parameter_list(JackTokenizer::new("int x, bool y")),
            Ok((
                vec![(Type::Int, "x".to_string()), (Type::Bool, "y".to_string())],
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_subroutine_declaration() {
        assert_eq!(
            subroutine_dec(JackTokenizer::new("function void foo() {}")),
            Ok((
                SubroutineDec(
                    SubroutineKind::Function,
                    Type::Void,
                    "foo".to_string(),
                    vec![],
                    SubroutineBody(vec![], vec![])
                ),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            subroutine_dec(JackTokenizer::new(
                "constructor void foo(int x, char y, bool z) {}"
            )),
            Ok((
                SubroutineDec(
                    SubroutineKind::Constructor,
                    Type::Void,
                    "foo".to_string(),
                    vec![
                        (Type::Int, "x".to_string()),
                        (Type::Char, "y".to_string()),
                        (Type::Bool, "z".to_string())
                    ],
                    SubroutineBody(vec![], vec![])
                ),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            subroutine_dec(JackTokenizer::new(
                "method int foo() {var int x; let x=42; return x;}"
            )),
            Ok((
                SubroutineDec(
                    SubroutineKind::Method,
                    Type::Int,
                    "foo".to_string(),
                    vec![],
                    SubroutineBody(
                        vec![VarDec::new(Type::Int, vec!["x"])],
                        vec![
                            Statement::let_("x", None, 42),
                            Statement::Return(Some(Term::variable("x").into()))
                        ]
                    )
                ),
                JackTokenizer::end()
            ))
        );
    }

    #[test]
    fn parse_classvar_declaration() {
        assert_eq!(
            classvar_dec(JackTokenizer::new("static int foo;")),
            Ok((
                ClassVarDec::Static(Type::Int, vec!["foo".to_string()]),
                JackTokenizer::end()
            ))
        );

        assert_eq!(
            classvar_dec(JackTokenizer::new("field int bar;")),
            Ok((
                ClassVarDec::Field(Type::Int, vec!["bar".to_string()]),
                JackTokenizer::end()
            ))
        );
        assert_eq!(
            classvar_dec(JackTokenizer::new("static int foo, bar;")),
            Ok((
                ClassVarDec::Static(Type::Int, vec!["foo".to_string(), "bar".to_string()]),
                JackTokenizer::end()
            ))
        );
    }
}
