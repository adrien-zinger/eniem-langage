/// Parser tree.
use nom_locate::*;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub struct Statement<'a> {
    pub pos: Span<'a>,
    pub inner: EStatement<'a>,
}

impl PartialEq<Statement<'_>> for Statement<'_> {
    fn eq(&self, other: &Statement) -> bool {
        other.inner == self.inner
    }
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub args: Vec<String>,
    pub inner: Compound<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Call<'a> {
    pub block_on: bool,
    pub params: Vec<Statement<'a>>,
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct Compound<'a> {
    pub block_on: bool,
    pub inner: Vec<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum EStatement<'a> {
    Function(Function<'a>),
    Str(String /* inner text */),
    Num(i32 /* inner number (signed) */),
    Compound(Compound<'a>),
    Copy(String /* variable name */),
    Ref(String /* variable name */),
    Call(Call<'a>),
    StdCall(Call<'a>),
    Operation(Box<Operation<'a>>),
}

#[derive(Debug)]
pub struct Expression<'a> {
    pub pos: Span<'a>,
    pub inner: EExpression<'a>,
}

impl<'a> Expression<'a> {
    pub fn is_blocking(&self) -> bool {
        let s = match &self.inner {
            EExpression::Assignation(a) => return a.block_on,
            EExpression::Declaration(a) => return a.block_on,
            EExpression::Statement(s) => s,
            EExpression::Module(_) => return false,
            EExpression::Using(_) => return false,
        };
        match &s.inner {
            EStatement::Compound(c) => c.block_on,
            EStatement::Call(c) => c.block_on,
            _ => false,
        }
    }
}

impl PartialEq<Expression<'_>> for Expression<'_> {
    fn eq(&self, other: &Expression) -> bool {
        other.inner == self.inner
    }
}

#[derive(Debug, PartialEq)]
pub struct Assignation<'a> {
    pub block_on: bool,
    pub var: String,
    pub to_assign: Statement<'a>,
    pub modify: bool,
}

#[derive(Debug, PartialEq)]
pub struct Module<'a> {
    pub name: String,
    pub inner: Compound<'a>,
}

#[derive(Debug, PartialEq)]
pub enum EExpression<'a> {
    Statement(Statement<'a>),
    Declaration(Assignation<'a>),
    Assignation(Assignation<'a>),
    Module(Module<'a>),
    Using(String),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    // /// ==
    // EqualEqual,
    // /// !=
    // NotEqual,
    // /// >
    // GreaterThan,
    // /// <
    // LesserThan,
    /// ! (unary operator)
    Not,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Mult,
    // /// &&
    // AndAnd,
    // /// ||,
    // OrOr,
    // /// &
    // BinaryAnd,
    // /// |
    // BinaryOr,
    /// Empty, nothing
    Empty,
}

#[derive(Debug, PartialEq)]
pub struct UnaryOperation<'a> {
    pub operator: Operator,
    pub statement: Statement<'a>,
}

/// Generic operation, a binary operation contains both
#[derive(Debug, PartialEq)]
pub enum Operation<'a> {
    Unary(UnaryOperation<'a>),
    Binary(Box<BinaryOperation<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct BinaryOperation<'a> {
    pub operator: Operator,
    pub right: Operation<'a>,
    pub left: Operation<'a>,
}
