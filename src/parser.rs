use crate::tree::*;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_till, take_until};
use nom::character::complete::{alpha1, alphanumeric1, anychar, char as cchar, digit1};
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::{not, opt, recognize, verify};
use nom::error::ParseError;
use nom::multi::{many0, many0_count, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;
use nom::Parser;
use nom_locate::position;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_parser")]
        std::println!($($rest)*)
    }
}

/// Space between tokens, which can content comments
fn spacing(s: Span) -> IResult<Span, ()> {
    let (s, _) = delimited(multispace0, opt(multiline_comments), multispace0).parse(s)?;
    let (s, _) = multispace0(s)?;
    Ok((s, ()))
}

/// Parse a multiline comment starting with "/*" and finishing with a "*/"
fn multiline_comments(s: Span) -> IResult<Span, ()> {
    let (s, _) = tag("/*")(s)?;
    debug!("enter spacing commented {:?}", s);
    let (s, _) = take_until("*/")(s)?;
    debug!("end spacing commented {:?}", s);
    let (s, _) = tag("*/")(s)?;
    debug!("pass end spacing commented {:?}", s);
    Ok((s, ()))
}

fn check_tag(t: &str, s: Span) -> bool {
    return tag::<&str, nom_locate::LocatedSpan<&str>, ()>(t)(s).is_ok();
}

/// Parse a right extension to a statement.
/// It should accept parenthesis (for function calls), bracket for lists etc.
/// Right now, only function call (extension of size 1) is supported.
fn extension(s: Span) -> IResult<Span, u8> {
    let (s, _) = spacing(s)?;
    if let Ok((s, _)) = tag::<&str, nom_locate::LocatedSpan<&str>, ()>("(")(s) {
        debug!("extension entry tag '('");
        let (s, _args) = separated_list0(tag(","), delimited(spacing, alpha1, spacing)).parse(s)?;
        let (s, _) = tag(")")(s)?;
        return Ok((s, 1));
    }
    debug!("direct return extension empty");
    Ok((s, 0))
}

/// A statement can split into a function, a compound, a branch or an
/// operation. Notice that the operation can split into any other
/// primitive statement in `primitive_operation_statement`.
fn statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter statement");
    debug!("statement fragment: {}", s.fragment());

    let case1 = opt(alt((function_statement, compound_statement))).parse(s)?;
    if let (s, Some(statement)) = case1 {
        debug!("function or compound statement found");
        let (s, _exts) = many0(extension).parse(s).unwrap_or((s, vec![]));
        debug!("extensions: {:?}", _exts);
        let (s, _) = opt(tag(";")).parse(s)?;
        return Ok((s, statement));
    }

    let s = case1.0;
    debug!("statement case 2, fragment: {}", s.fragment());
    let (s, statement) = alt((branch_statement, operation_statement)).parse(s)?;
    let (s, _exts) = many0(extension).parse(s).unwrap_or((s, vec![]));
    debug!("extensions: {:?}", _exts);
    let (s, _) = opt(tag(";")).parse(s)?;
    let (s, _) = spacing(s)?;
    debug!("return statement {}", s.fragment());
    Ok((s, statement))
}

/// The branch statement is the "if" statement
/// which split the execution into two possible
/// way.
fn branch_statement(s: Span) -> IResult<Span, Statement> {
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, _) = tag("if")(s)?;
    let (s, condition) = operation_statement(s)?;
    let (s, left) = compound_statement(s)?;
    let (s, _) = spacing(s)?;
    let (s, _) = tag("else")(s)?;
    let (s, _) = spacing(s)?;
    let (s, right) = compound_statement(s)?;
    let branch = Branch {
        condition,
        left,
        right,
    };
    let res = Statement {
        pos,
        inner: EStatement::Branch(branch.into()),
    };
    Ok((s, res))
}

fn expression_statement(s: Span) -> IResult<Span, Expression> {
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, statement) = statement(s)?;
    let res = Expression {
        pos,
        inner: EExpression::Statement(statement),
    };
    Ok((s, res))
}

#[test]
fn test_statement() {
    // Function
    assert!(statement(Span::new("(){}")).is_ok());
    assert!(statement(Span::new("(){};")).is_ok());
    // String
    assert!(statement(Span::new("\"\";")).is_ok());
    assert!(statement(Span::new("\"abc\";")).is_ok());
    assert!(statement(Span::new("\"\"")).is_ok());
    // call
    assert!(statement(Span::new("foo();")).is_ok());
    assert!(statement(Span::new("foo()")).is_ok());
}

fn copy_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter copy {}", s.fragment());
    let (s, _) = delimited(spacing, tag("copy"), spacing).parse(s)?;
    let (s, var) = variable_name(s)?;
    let (s, pos) = position(s)?;
    let res = Statement {
        pos,
        inner: EStatement::Copy(var.to_string()),
    };

    Ok((s, res))
}

fn ref_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter ref {}", s.fragment());
    let (s, pos) = position(s)?;
    let (s, _) = spacing(s)?;
    let (s, _) = opt(tag("ref")).parse(s)?;
    let (s, _) = spacing(s)?;

    let check = not(tag::<&str, nom_locate::LocatedSpan<&str>, ()>("let")).parse(s);

    if check.is_err() {
        println!("Syntax error at {:?}, {:?}, {}", pos, s, pos.get_column());
    }

    let (s, var) = variable_name(s)?;
    let (s, pos) = position(s)?;
    let res = Statement {
        pos,
        inner: EStatement::Ref(var.to_string()),
    };
    debug!("return ref");
    Ok((s, res))
}

fn string_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter string {}", s.fragment());
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, string) = quoted_string(s)?;
    let (s, _) = spacing(s)?;
    debug!("string, fragment: {}", s.fragment());
    let res = Statement {
        pos,
        inner: EStatement::Str(string.to_string()),
    };
    debug!("return string");
    Ok((s, res))
}

fn quoted_string(s: Span) -> IResult<Span, String> {
    let (s, _) = tag("\"")(s)?;
    let mut res = String::new();
    let (mut s, mut r) = take_till(|c| c == '\\' || c == '\"').parse(s)?;
    loop {
        res.push_str(r.fragment());
        if let (s, Some(_)) = opt(tag("\"")).parse(s)? {
            return Ok((s, res));
        }
        (s, _) = tag("\\")(s)?;
        let (s2, c) = anychar(s)?;
        match c {
            'n' => res.push('\n'),
            'r' => res.push('\r'),
            '\\' => res.push('\\'),
            '\"' => res.push('\"'),
            '\'' => res.push('\''),
            _ => todo!("add an escape character in parser {}", c),
        }
        (s, r) = take_till(|c| c == '\\' || c == '\"').parse(s2)?;
    }
}

#[test]
fn test_string() {
    // Basic cases
    assert_eq!(quoted_string(Span::new("\"\"")).unwrap().1, "");
    assert_eq!(quoted_string(Span::new("\"abc\"")).unwrap().1, "abc");
    assert_eq!(quoted_string(Span::new("\"a b,c@\"")).unwrap().1, "a b,c@");
    assert_eq!(quoted_string(Span::new("\"a\\\\b\"")).unwrap().1, "a\\b");
    assert_eq!(quoted_string(Span::new("\"a\\\"b\"")).unwrap().1, "a\"b");
    assert_eq!(quoted_string(Span::new("\"\\n\"")).unwrap().1, "\n");
}

/// Parse a constant number.
/// It is a basic statement.
fn num_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter num {}", s.fragment());
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, num) = preceded(opt(tag("-")), digit1).parse(s)?;
    let (s, _) = spacing(s)?;
    debug!("num, fragment: {}", s.fragment());
    let res = Statement {
        pos,
        inner: EStatement::Num(num.to_string().parse().unwrap()),
    };
    debug!("return num");
    Ok((s, res))
}

/// Parse a constant boolean
fn bool_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter bool {}", s.fragment());
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, val) = alt((tag("true"), tag("false"))).parse(s)?;
    alt((multispace1, tag(")"), tag(","))).parse(s)?;
    let (s, _) = spacing(s)?;
    debug!("bool, fragment: {}", s.fragment());
    let val = val.to_string() == "true";
    let res = Statement {
        pos,
        inner: EStatement::Bool(val),
    };
    debug!("return bool");
    Ok((s, res))
}

fn is_compound_satement(s: Span) -> IResult<Span, ()> {
    let (s, _) = opt(delimited(spacing, tag("await"), spacing)).parse(s)?;
    return delimited(spacing, tag("{"), spacing)
        .parse(s)
        .map(|_| (s, ()));
}

fn compound_statement(s: Span) -> IResult<Span, Statement> {
    let (s, block_on) = opt(delimited(spacing, tag("await"), spacing)).parse(s)?;
    let (s, _) = delimited(spacing, tag("{"), spacing).parse(s)?;
    let (s, pos) = position(s)?;
    debug!("look for expressions in compound {:?}", s);
    let (s, inner) = opt(expressions)
        .parse(s)
        .unwrap_or_else(|_| (s, Some(vec![])));
    debug!("expressions parsed\n{:#?}", inner);
    let (s, _) = delimited(spacing, tag("}"), spacing).parse(s)?;
    debug!("compound statement");
    let compound = Compound {
        inner: inner.unwrap_or_else(std::vec::Vec::new),
        block_on: block_on.is_some(),
    };

    let res = Statement {
        pos,
        inner: EStatement::Compound(compound),
    };

    Ok((s, res))
}

#[test]
fn test_compound_statement() {
    // Basic cases
    assert!(compound_statement(Span::new("{}")).is_ok());
    assert!(compound_statement(Span::new("ab")).is_err());
    assert!(compound_statement(Span::new("")).is_err());
    compound_statement(Span::new("{ let a = (){ \"bar\" } a }")).unwrap();
}

/// Parsed after "::" in a variable name
pub fn variable_subname(s: Span) -> IResult<Span, nom_locate::LocatedSpan<&str>> {
    let (s, _) = not(tag("true")).parse(s)?;
    let (s, _) = not(tag("false")).parse(s)?;
    let (s, _) = not(tag("in")).parse(s)?;
    let (s, _) = not(tag("if")).parse(s)?;
    let (s, _) = not(tag("as")).parse(s)?;
    let (s, _) = not(tag("mod")).parse(s)?;
    let (s, _) = not(tag("let")).parse(s)?;
    let (s, _) = not(tag("use")).parse(s)?;
    let (s, _) = not(tag("for")).parse(s)?;
    let (s, _) = not(tag("else")).parse(s)?;
    alphanumeric1(s)
}

/// Parse a variable name
pub fn variable_name(s: Span) -> IResult<Span, String> {
    let (s, _) = spacing(s)?;
    let (s, _) = not(tag("true")).parse(s)?;
    let (s, _) = not(tag("false")).parse(s)?;
    let (s, _) = not(tag("in")).parse(s)?;
    let (s, _) = not(tag("if")).parse(s)?;
    let (s, _) = not(tag("else")).parse(s)?;
    let (s, _) = not(tag("as")).parse(s)?;
    let (s, _) = not(tag("mod")).parse(s)?;
    let (s, _) = not(tag("let")).parse(s)?;
    let (s, _) = not(tag("use")).parse(s)?;
    let (s, _) = not(tag("for")).parse(s)?;
    let (s, _) = not(tag("ref")).parse(s)?;
    let (s, _) = not(tag("copy")).parse(s)?;
    let (s, varname) = recognize(pair(
        verify(anychar, |&c| c.is_lowercase()),
        many0_count(preceded(opt(cchar('_')), variable_subname)),
    ))
    .parse(s)?;
    let (s, _) = spacing(s)?;
    // todo, make keywords throw warnings
    Ok((s, varname.fragment().to_string()))
}

#[test]
fn test_variable_name() {
    assert!(variable_name(Span::new("  a  ")).is_ok());
    assert!(variable_name(Span::new("foobar")).is_ok());
    assert!(variable_name(Span::new("foo_bar")).is_ok());
    let (s, foo32_bar) = variable_name(Span::new("foo32_bar")).unwrap();
    assert_eq!(foo32_bar.to_string(), "foo32_bar");
    assert!(s.fragment().is_empty());
    let (s, not_equal) = variable_name(Span::new("not_equal")).unwrap();
    assert_eq!(not_equal.to_string(), "not_equal");
    assert!(s.fragment().is_empty());
    assert!(variable_name(Span::new("32foo")).is_err());
    assert!(variable_name(Span::new("_32foo")).is_err());

    // Reserved world should throw an error
    assert!(variable_name(Span::new("true")).is_err());
    assert!(variable_name(Span::new("false")).is_err());
    assert!(variable_name(Span::new("let")).is_err());
    assert!(variable_name(Span::new("if")).is_err());
    assert!(variable_name(Span::new("else")).is_err());
}

pub fn function_statement_parameter(s: Span) -> IResult<Span, String> {
    let (s, pos) = position(s)?;
    let vname = variable_name(s);
    if vname.is_err() {
        println!("spot real error");
        return Ok((s, String::from("ref")));
    }
    vname
}

pub fn function_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter function");
    println!("function span: {:?}", s);
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, _) = tag("(")(s)?;
    debug!("try to read parameters span: {:?}", s);
    let (s, args) = separated_list0(
        tag(","),
        delimited(spacing, function_statement_parameter, spacing),
    )
    .parse(s)?;
    debug!("succeed to read parameters span: {:?}", s);
    let (s, _) = tag(")")(s)?;
    let (s, _) = spacing(s)?;
    debug!("function statement enter in compound");
    let (s, body) = compound_statement(s)?;
    debug!("function statement quit compound");
    let body = if let EStatement::Compound(body) = body.inner {
        body
    } else {
        unreachable!()
    };

    debug!("function statement, fragment: {}", s.fragment());
    let function = Function {
        args: args.iter().map(|s| s.to_string()).collect(),
        inner: body,
    };

    let res = Statement {
        pos,
        inner: EStatement::Function(function),
    };

    Ok((s, res))
}

#[test]
fn test_function_statement() {
    // Basic cases
    assert!(function_statement(Span::new("(){}")).is_ok());
    assert!(function_statement(Span::new("()   {}")).is_ok());
    assert!(function_statement(Span::new("(a){}")).is_ok());
    assert!(function_statement(Span::new("(  a   ){}")).is_ok());
    assert!(function_statement(Span::new("(a,b){}")).is_ok());
    assert!(function_statement(Span::new("(  a  ,  b  ){}")).is_ok());
    assert!(function_statement(Span::new("(){ let a = \"abc\"; }")).is_ok());
    assert!(function_statement(Span::new("){}")).is_err());
    assert!(function_statement(Span::new("({}")).is_err());

    /* those errors are accepted somehow */
    assert!(function_statement(Span::new("(,){}")).is_ok());
    assert!(function_statement(Span::new("(  ,,){}")).is_ok());
}

/// Parse function declaration
pub fn declaration(s: Span) -> IResult<Span, Expression> {
    debug!("enter declaration {}", s.fragment());
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, block_on) = opt(delimited(spacing, tag("await"), spacing)).parse(s)?;
    let (s, _) = tag("let")(s)?;
    debug!("enter declaration let confirmed {}", s.fragment());
    let (s, _) = multispace1(s)?;
    debug!("let space confirmed");
    let (s, _) = spacing(s)?;
    if variable_subname(s).is_err() {
        println!("invalid variable name {:?}", s);
    }

    debug!("var name confirmed");
    let (s, var) = delimited(spacing, variable_name, spacing).parse(s)?;
    debug!("var name confirmed again");
    debug!("span with '=' expected {}", s.fragment());

    if tag::<_, _, nom::error::Error<_>>("=")(s).is_err() {
        println!(" the = was very expected here");
    }

    let (s, _) = tag("=")(s)?;
    debug!("declaration, try to parse right: {}", s.fragment());
    let (s, to_assign) = delimited(spacing, statement, spacing).parse(s)?;
    debug!("declaration, fragment: {}", s.fragment());

    let assignation = Assignation {
        block_on: block_on.is_some(),
        var: var.to_string(),
        to_assign,
        modify: true,
    };

    debug!("return declaration");
    let res = Expression {
        pos,
        inner: EExpression::Declaration(assignation),
    };
    debug!("return declaration:\n {:?}", res);
    Ok((s, res))
}

#[test]
fn test_declaration() {
    // Basic cases
    assert!(declaration(Span::new("let a = (){}")).is_ok());
    assert!(declaration(Span::new("let a = {}")).is_ok());
    assert!(declaration(Span::new("let a = \"abc\";")).is_ok());
    assert!(declaration(Span::new("let a = \"abc\"")).is_ok());
    assert!(declaration(Span::new("let a = \"abc \\\" 001  hW\";")).is_ok());
    assert!(declaration(Span::new("let a = variable;")).is_ok());
    assert!(declaration(Span::new("let a = copy variable;")).is_ok());
    assert!(declaration(Span::new("let a = ref variable;")).is_ok());
    assert!(declaration(Span::new("let a){}")).is_err());
    assert!(declaration(Span::new("({}")).is_err());
}

pub fn assignation(s: Span) -> IResult<Span, Expression> {
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, block_on) = opt(delimited(spacing, tag("await"), spacing)).parse(s)?;
    let (s, var) = variable_name(s)?;
    let (s, modify) = opt(delimited(spacing, tag(":"), spacing)).parse(s)?;
    let (s, _) = tag("=")(s)?;
    debug!("assignation");
    let (s, to_assign) = delimited(spacing, statement, spacing).parse(s)?;

    let assignation = Assignation {
        block_on: block_on.is_some(),
        var: var.to_string(),
        to_assign,
        modify: modify.is_some(),
    };
    let res = Expression {
        pos,
        inner: EExpression::Assignation(assignation),
    };
    Ok((s, res))
}

#[test]
fn test_assignation() {
    assert!(assignation(Span::new("a = (){}")).is_ok());
    assert!(assignation(Span::new("await a = \"abc\";")).is_ok());
    assert!(assignation(Span::new("a = foo();")).is_ok());
    assert!(assignation(Span::new("a = ref b;")).is_ok());
    assert!(assignation(Span::new("a = ref b")).is_ok());
    assert!(assignation(Span::new("a = \"hello\"")).is_ok());
    assert!(assignation(Span::new("a = 0")).is_ok());
    assert!(assignation(Span::new("a = -12")).is_ok());
}

pub fn param_statement(s: Span) -> IResult<Span, Statement> {
    alt((
        branch_statement,
        operation_statement,
        function_statement,
        compound_statement,
        copy_statement,
        string_statement,
        num_statement,
        bool_statement,
        call_statement, /* to check before ref, because it's ref + "("... */
        ref_statement,
    ))
    .parse(s)
}

pub fn call_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter call {}", s.fragment());
    let (s, pos) = position(s)?;
    let (s, block_on) = opt(delimited(spacing, tag("await"), multispace1)).parse(s)?;
    // if there is no await tag, ensure that function name is not "await"
    let (s, _) = not(tag("await")).parse(s)?;
    let (s, name) = delimited(spacing, variable_name, spacing).parse(s)?;
    let (s, std) = opt(tag("!")).parse(s)?;
    let (s, _) = delimited(spacing, tag("("), spacing).parse(s)?;
    let (s, params) =
        separated_list0(tag(","), delimited(spacing, param_statement, spacing)).parse(s)?;
    let (s, _) = delimited(spacing, tag(")"), spacing).parse(s)?;
    let call = Call {
        params,
        block_on: block_on.is_some(),
        name: name.to_string(),
    };

    let res = Statement {
        pos,
        inner: if std.is_some() {
            EStatement::StdCall(call)
        } else {
            EStatement::Call(call)
        },
    };

    debug!("return call");
    Ok((s, res))
}

#[test]
fn test_call_statement() {
    // assert!(call_statement(Span::new("foo()")).is_ok());
    // assert!(call_statement(Span::new("foo(a, b)")).is_ok());
    // assert!(call_statement(Span::new("foo((){}, b)")).is_ok());
    // assert!(call_statement(Span::new("foo(\"abc\", b)")).is_ok());
    assert!(call_statement(Span::new("foo  (copy a, copy b)")).is_ok());
    // assert!(call_statement(Span::new("await foo()")).is_ok());
    // assert!(call_statement(Span::new("await()")).is_err());
    let (_, call) = call_statement(Span::new("foo!()")).unwrap();
    match call.inner {
        EStatement::StdCall(_) => {}
        _ => panic!("unexpected call"),
    }
}

/// Primitive unit of an operation which is:
///    - case1, a statement (wo operation case)
///    - case2, a statement between parenthesis
pub fn primitive_operation_statement(s: Span) -> IResult<Span, Statement> {
    let (s, _) = spacing(s)?;

    if check_tag("if", s) {
        return branch_statement(s);
    }

    if check_tag("(", s) {
        if let Ok(res) = function_statement(s) {
            return Ok(res);
        } else {
            let (s, _) = tag("(")(s)?;
            let (s, _) = spacing(s)?;
            let (s, statement) = statement(s)?;
            let (s, _) = spacing(s)?;
            let (s, _) = tag(")")(s)?;
            return Ok((s, statement));
        }
    }

    if check_tag("{", s) {
        return compound_statement(s);
    }

    if check_tag("copy", s) {
        return copy_statement(s);
    }

    if check_tag("ref", s) {
        return ref_statement(s);
    }

    if variable_name(s).is_ok() {
        if let Ok(res) = call_statement(s) {
            return Ok(res);
        } else {
            return ref_statement(s);
        }
    }

    alt((string_statement, num_statement, bool_statement)).parse(s)
}

/// Unary operation as 'not X'.
pub fn unary_operation(s: Span) -> IResult<Span, UnaryOperation> {
    let (s, not) = opt(tag("!")).parse(s)?;
    if not.is_some() {
        let (s, es) = primitive_operation_statement(s)?;
        Ok((
            s,
            UnaryOperation {
                operator: Operator::Not,
                statement: es,
            },
        ))
    } else {
        let (s, es) = primitive_operation_statement(s)?;
        Ok((
            s,
            UnaryOperation {
                operator: Operator::Empty,
                statement: es,
            },
        ))
    }
}

/// Parse a sequence of binary operation "a * b * ...".
/// Fallback to unary operation if no such '*'.
pub fn mult_operation(s: Span) -> IResult<Span, Operation> {
    let (s, _) = spacing(s)?;
    let (s, right) = unary_operation(s)?;
    let (s, _) = spacing(s)?;
    let (s, is_mult) = opt(tag("*")).parse(s)?;
    if is_mult.is_none() {
        return Ok((s, Operation::Unary(right)));
    }
    let (s, _) = spacing(s)?;
    let (s, left) = mult_operation(s)?;
    Ok((
        s,
        Operation::Binary(Box::new(BinaryOperation {
            operator: Operator::Mult,
            right: Operation::Unary(right),
            left,
        })),
    ))
}

/// Parse a sequence of binary operation "a + b + ..." (+ or -).
/// Fallback to mult operation if no such '+'.
pub fn add_operation(s: Span) -> IResult<Span, Operation> {
    let (s, _) = spacing(s)?;
    let (s, right) = mult_operation(s)?;
    let (s, _) = spacing(s)?;
    let (s, operator) = opt(alt((tag("-"), tag("+"), tag("=="), tag("!=")))).parse(s)?;
    if operator.is_none() {
        return Ok((s, right));
    }
    let (s, _) = spacing(s)?;
    let (s, left) = add_operation(s)?;
    let operator = match *operator.unwrap().fragment() {
        "-" => Operator::Minus,
        "+" => Operator::Plus,
        "==" => Operator::EqualEqual,
        "!=" => Operator::NotEqual,
        _ => unreachable!(),
    };
    debug!("successfuly found an operation");
    Ok((
        s,
        Operation::Binary(Box::new(BinaryOperation {
            operator,
            right,
            left,
        })),
    ))
}

/// Try to parse an operation. It can be arithmetic or logic.
/// This is the top of operation parsing.
///
/// Can be used as function parameter, right part of an assignation,
/// into a branch test.
pub fn operation_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter in operation statement");
    let (s, op) = add_operation(s)?;
    let (s, pos) = position(s)?;
    if let Operation::Unary(op) = op {
        if let Operator::Empty = op.operator {
            Ok((
                s,
                Statement {
                    pos,
                    inner: match op.statement.inner {
                        EStatement::Ref(r) => EStatement::Ref(r.clone()),
                        EStatement::Str(r) => EStatement::Str(r.clone()),
                        EStatement::Num(r) => EStatement::Num(r.clone()),
                        EStatement::Function(r) => EStatement::Function(r),
                        EStatement::Bool(r) => EStatement::Bool(r.clone()),
                        EStatement::Compound(r) => EStatement::Compound(r),
                        EStatement::Copy(r) => EStatement::Copy(r.clone()),
                        EStatement::Call(r) => EStatement::Call(r),
                        EStatement::StdCall(r) => EStatement::StdCall(r),
                        EStatement::Branch(r) => EStatement::Branch(r),
                        EStatement::Operation(r) => EStatement::Operation(r),
                    },
                },
            ))
        } else {
            Ok((
                s,
                Statement {
                    pos,
                    inner: EStatement::Operation(Box::new(Operation::Unary(op))),
                },
            ))
        }
    } else {
        Ok((
            s,
            Statement {
                pos,
                inner: EStatement::Operation(Box::new(op)),
            },
        ))
    }
}

#[test]
fn test_operation() {}

fn ext_path(s: Span) -> IResult<Span, String> {
    let (s, path) =
        separated_list1(delimited(spacing, tag("::"), spacing), variable_name).parse(s)?;
    let (s, _) = not(tag("::")).parse(s)?;
    Ok((s, path.join("::")))
}

fn using(s: Span) -> IResult<Span, Expression> {
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, _) = tag("use")(s)?;
    let (s, path) = delimited(multispace1, ext_path, spacing).parse(s)?;
    let (s, _) = opt(tag(";")).parse(s)?;
    Ok((
        s,
        Expression {
            pos,
            inner: EExpression::Using(path),
        },
    ))
}

fn module(s: Span) -> IResult<Span, Expression> {
    let (s, _) = spacing(s)?;
    let (s, pos) = position(s)?;
    let (s, _) = tag("mod")(s)?;
    let (s, _) = multispace1(s)?;
    let (s, name) = variable_name(s)?;
    let (s, _) = spacing(s)?;
    let (s, statement) = compound_statement(s)?;
    let (s, _) = spacing(s)?;
    let (s, _) = opt(tag(";")).parse(s)?;
    if let EStatement::Compound(inner) = statement.inner {
        Ok((
            s,
            Expression {
                pos,
                inner: EExpression::Module(Module { inner, name }),
            },
        ))
    } else {
        unreachable!()
    }
}

#[test]
fn test_module() {
    assert!(module(Span::new("mod a { let a = i32_add(2, 2) }")).is_ok());
    assert!(module(Span::new("mod a { mod b { let a = i32_add(2, 2) } }")).is_ok());
    assert!(module(Span::new("mod a{mod b{ let a = i32_add(2, 2) } }")).is_ok());
}

#[test]
fn test_using() {
    assert!(using(Span::new("use a::b::c;")).is_ok());
    assert!(using(Span::new("use b")).is_ok());
    assert!(using(Span::new("use ::b")).is_err());
    assert!(using(Span::new("use b::")).is_err());
}

/// Parse a list of expression (at least one)
pub fn expressions(s: Span) -> IResult<Span, Vec<Expression>> {
    many1(alt((
        declaration,
        assignation,
        module,
        using,
        expression_statement,
    )))
    .parse(s)
}

#[test]
fn test_expressions() {
    assert!(expressions(Span::new("let /* I will declare a */ a = \"\"")).is_ok());
    //assert!(expressions(Span::new("a = (){}")).is_ok());
    //assert!(expressions(Span::new("let a = \"\"")).is_ok());
    //assert!(expressions(Span::new("let a = \"\";")).is_ok());
    //assert!(expressions(Span::new("let a = (){}; let b = \"hello world\";")).is_ok());
    //assert!(expressions(Span::new("let a = (){}; let b = \"hello world\";")).is_ok());
    //assert!(expressions(Span::new(
    //    "let a = (){ let bar = \"\"; } let b = \"hello world\";"
    //))
    //.is_ok());
    //assert!(expressions(Span::new(
    //    "let a = (){ let bar = \"\" } let b = \"hello world\";"
    //))
    //.is_ok());
}

#[test]
fn test_code_1() {
    let input = "
		let a = \"abc\";
		printf(a);
	";

    let ast = expressions(Span::new(input));
    assert!(ast.is_ok());
    let ast = ast.unwrap().1;
    assert_eq!(ast.len(), 2);
}

#[test]
fn test_code_2() {
    let input = "
	let foo = (){ \"bar\"; }
	let a = {
		let foo = (){ \"barbar\" };
		foo()
	}";

    let ast = expressions(Span::new(input));
    assert!(ast.is_ok());
    let ast = ast.unwrap().1;
    assert_eq!(ast.len(), 2);
}
