use crate::tree::*;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, anychar, digit1};
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::{not, opt};
use nom::multi::{many0, many1, many_till, separated_list0};
use nom::sequence::{delimited, preceded};
use nom::IResult;
use nom::Parser;
use nom_locate::position;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_parser")]
        std::println!($($rest)*)
    }
}

fn extension(s: Span) -> IResult<Span, u8> {
    let (s, _) = multispace0(s)?;
    if let Ok((s, _)) = tag::<&str, nom_locate::LocatedSpan<&str>, ()>("(")(s) {
        debug!("extension entry tag '('");
        let (s, _args) =
            separated_list0(tag(","), delimited(multispace0, alpha1, multispace0)).parse(s)?;
        let (s, _) = tag(")")(s)?;
        return Ok((s, 1));
    }
    debug!("direct return extension empty");
    Ok((s, 0))
}

fn statement(s: Span) -> IResult<Span, Expression> {
    // case 1: ';' termination is optional
    let (s, _) = multispace0(s)?;
    let (s, pos) = position(s)?;
    let case1 = opt(alt((function_statement, compound_statement))).parse(s)?;
    if let (s, Some(statement)) = case1 {
        debug!("function or compound statement found");
        let (s, exts) = many0(extension).parse(s).unwrap_or((s, vec![]));
        debug!("extensions: {:?}", exts);
        let (s, _) = opt(tag(";")).parse(s)?;
        let res = Expression {
            pos,
            inner: EExpression::Statement(statement),
        };

        return Ok((s, res));
    }

    let s = case1.0;
    debug!("statement case 2, fragment: {}", s.fragment());
    let (s, statement) = alt((
        copy_statement,
        call_statement, /* to check before ref, because it's ref + "("... */
        ref_statement,
        string_statement,
        num_statement,
    ))
    .parse(s)?;

    let (s, exts) = many0(extension).parse(s).unwrap_or((s, vec![]));
    debug!("extensions: {:?}", exts);
    let (s, _) = opt(tag(";")).parse(s)?;

    let (s, _) = multispace0(s)?;
    debug!("return statement {}", s.fragment());
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
    let (s, _) = delimited(multispace0, tag("copy"), multispace0).parse(s)?;
    let (s, var) = alpha1(s)?;
    let (s, pos) = position(s)?;
    let res = Statement {
        pos,
        inner: EStatement::Copy(var.to_string()),
    };

    Ok((s, res))
}

fn ref_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter ref {}", s.fragment());
    let (s, _) = multispace0(s)?;
    let (s, _) = not(tag("let")).parse(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = opt(tag("ref")).parse(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = not(tag::<&str, nom_locate::LocatedSpan<&str>, ()>("let"))
        .parse(s)
        .unwrap();
    let (s, var) = alpha1(s)?;
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
    let (s, _) = multispace0(s)?;
    let (s, pos) = position(s)?;
    let (s, string) = quoted_string(s)?;
    let (s, _) = multispace0(s)?;
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
    let (mut s, (mut r, mut d)) = many_till(anychar, alt((tag("\\"), tag("\"")))).parse(s)?;
    loop {
        res.push_str(&r.iter().collect::<String>());
        if *d.fragment() == "\"" {
            return Ok((s, res));
        }
        res.push('\\');
        let (s2, c) = anychar(s)?;
        res.push(c);
        (s, (r, d)) = many_till(anychar, alt((tag("\\"), tag("\"")))).parse(s2)?;
    }
}

#[test]
fn test_string() {
    // Basic cases
    assert_eq!(quoted_string(Span::new("\"\"")).unwrap().1, "");
    assert_eq!(quoted_string(Span::new("\"abc\"")).unwrap().1, "abc");
    assert_eq!(quoted_string(Span::new("\"a b,c@\"")).unwrap().1, "a b,c@");
    assert_eq!(quoted_string(Span::new("\"a\\b\"")).unwrap().1, "a\\b");
    assert_eq!(quoted_string(Span::new("\"a\\\"b\"")).unwrap().1, "a\\\"b");
}

fn num_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter string {}", s.fragment());
    let (s, _) = multispace0(s)?;
    let (s, pos) = position(s)?;
    let (s, num) = preceded(opt(tag("-")), digit1).parse(s)?;
    let (s, _) = multispace0(s)?;
    debug!("num, fragment: {}", s.fragment());
    let res = Statement {
        pos,
        inner: EStatement::Num(num.to_string().parse().unwrap()),
    };
    debug!("return string");
    Ok((s, res))
}

fn compound_statement(s: Span) -> IResult<Span, Statement> {
    let (s, block_on) = opt(delimited(multispace0, tag("await"), multispace0)).parse(s)?;
    let (s, _) = delimited(multispace0, tag("{"), multispace0).parse(s)?;
    let (s, pos) = position(s)?;
    debug!("look for expressions in compound {:?}", s);
    let (s, inner) = opt(expressions)
        .parse(s)
        .unwrap_or_else(|_| (s, Some(vec![])));
    debug!("expressions parsed\n{:#?}", inner);
    let (s, _) = delimited(multispace0, tag("}"), multispace0).parse(s)?;
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

fn function_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter function");
    let (s, _) = multispace0(s)?;
    let (s, pos) = position(s)?;
    let (s, _) = tag("(")(s)?;
    let (s, args) =
        separated_list0(tag(","), delimited(multispace0, alpha1, multispace0)).parse(s)?;
    let (s, _) = tag(")")(s)?;
    let (s, _) = multispace0(s)?;
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
    assert!(function_statement(Span::new("(,){}")).is_err());
    assert!(function_statement(Span::new("(  ,,){}")).is_err());
    assert!(function_statement(Span::new("(  a   ){}")).is_ok());
    assert!(function_statement(Span::new("(a,b){}")).is_ok());
    assert!(function_statement(Span::new("(  a  ,  b  ){}")).is_ok());
    assert!(function_statement(Span::new("(){ let a = \"abc\"; }")).is_ok());
    assert!(function_statement(Span::new("){}")).is_err());
    assert!(function_statement(Span::new("({}")).is_err());
}

/// Parse function declaration
fn declaration(s: Span) -> IResult<Span, Expression> {
    debug!("enter declaration {}", s.fragment());
    let (s, _) = multispace0(s)?;
    let (s, pos) = position(s)?;
    let (s, block_on) = opt(delimited(multispace0, tag("await"), multispace0)).parse(s)?;
    let (s, _) = tag("let")(s)?;
    let (s, var) = delimited(multispace1, alpha1, multispace0).parse(s)?;
    let (s, _) = tag("=")(s)?;
    let (s, expr) = delimited(multispace0, statement, multispace0).parse(s)?;
    debug!("declaration, fragment: {}", s.fragment());

    let to_assign = if let EExpression::Statement(to_assign) = expr.inner {
        to_assign
    } else {
        unreachable!()
    };

    let assignation = Assignation {
        block_on: block_on.is_some(),
        var: var.to_string(),
        to_assign,
    };

    debug!("return declaration");
    let res = Expression {
        pos,
        inner: EExpression::Declaration(assignation),
    };
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

fn assignation(s: Span) -> IResult<Span, Expression> {
    let (s, _) = multispace0(s)?;
    let (s, pos) = position(s)?;
    let (s, block_on) = opt(delimited(multispace0, tag("await"), multispace0)).parse(s)?;
    let (s, var) = delimited(multispace0, alpha1, multispace0).parse(s)?;
    let (s, _) = tag("=")(s)?;
    debug!("assignation");
    let (s, expr) = delimited(multispace0, statement, multispace0).parse(s)?;

    let to_assign = if let EExpression::Statement(to_assign) = expr.inner {
        to_assign
    } else {
        unreachable!()
    };

    let assignation = Assignation {
        block_on: block_on.is_some(),
        var: var.to_string(),
        to_assign,
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

fn param_statement(s: Span) -> IResult<Span, Statement> {
    alt((
        function_statement,
        compound_statement,
        copy_statement,
        call_statement, /* to check before ref, because it's ref + "("... */
        ref_statement,
        string_statement,
        num_statement,
    ))
    .parse(s)
}

fn call_statement(s: Span) -> IResult<Span, Statement> {
    debug!("enter call {}", s.fragment());
    let (s, pos) = position(s)?;
    let (s, block_on) = opt(delimited(multispace0, tag("await"), multispace1)).parse(s)?;
    // if there is no await tag, ensure that function name is not "await"
    let (s, _) = not(tag("await")).parse(s)?;
    let (s, name) = delimited(multispace0, alpha1, multispace0).parse(s)?;
    let (s, std) = opt(tag("!")).parse(s)?;
    let (s, _) = delimited(multispace0, tag("("), multispace0).parse(s)?;
    let (s, params) = separated_list0(
        tag(","),
        delimited(multispace0, param_statement, multispace0),
    )
    .parse(s)?;
    let (s, _) = delimited(multispace0, tag(")"), multispace0).parse(s)?;
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
    assert!(call_statement(Span::new("foo()")).is_ok());
    assert!(call_statement(Span::new("foo(a, b)")).is_ok());
    assert!(call_statement(Span::new("foo((){}, b)")).is_ok());
    assert!(call_statement(Span::new("foo(\"abc\", b)")).is_ok());
    assert!(call_statement(Span::new("foo  (copy a, copy b)")).is_ok());
    assert!(call_statement(Span::new("await foo()")).is_ok());
    assert!(call_statement(Span::new("await()")).is_err());
}

/// Parse a list of expression (at least one)
pub fn expressions(s: Span) -> IResult<Span, Vec<Expression>> {
    let ret = many1(alt((declaration, assignation, statement))).parse(s);
    debug!("found one expression: {}", ret.is_ok());
    ret
}

#[test]
fn test_expressions() {
    assert!(expressions(Span::new("a = (){}")).is_ok());
    assert!(expressions(Span::new("let a = \"\"")).is_ok());
    assert!(expressions(Span::new("let a = \"\";")).is_ok());
    assert!(expressions(Span::new("let a = (){}; let b = \"hello world\";")).is_ok());
    assert!(expressions(Span::new("let a = (){}; let b = \"hello world\";")).is_ok());
    assert!(expressions(Span::new(
        "let a = (){ let bar = \"\"; } let b = \"hello world\";"
    ))
    .is_ok());
    assert!(expressions(Span::new(
        "let a = (){ let bar = \"\" } let b = \"hello world\";"
    ))
    .is_ok());
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

/* Some advanced tests */

#[test]
fn atest_assignation() {
    let a = assignation(Span::new("a = (){}"));
    debug!("{:#?}", a);

    let expected = Expression {
        pos: Span::new(""),
        inner: EExpression::Assignation(Assignation {
            block_on: false,
            var: "a".to_string(),
            to_assign: Statement {
                pos: Span::new(""),
                inner: EStatement::Function(Function {
                    args: vec![],
                    inner: Compound {
                        block_on: false,
                        inner: vec![],
                    },
                }),
            },
        }),
    };
    assert!(a.is_ok());
    assert_eq!(a.unwrap().1, expected);
}
