use nom_locate::*;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub struct Statement<'a> {
    pub pos: Span<'a>,
    pub inner: EStatement<'a>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub args: Vec<String>,
    pub inner: Compound<'a>,
}

#[derive(Debug)]
pub struct Call<'a> {
	pub block_on: bool,
    pub params: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub struct Compound<'a> {
    pub block_on: bool,
    pub inner: Vec<Expression<'a>>,
}

#[derive(Debug)]
pub enum EStatement<'a> {
    Function(Function<'a>),
    Str(String /* inner text */),
    Compound(Compound<'a>),
    Copy(String /* variable name */),
    Ref(String /* variable name */),
    Call(Call<'a>),
}

#[derive(Debug)]
pub struct Expression<'a> {
    pub pos: Span<'a>,
    pub inner: EExpression<'a>,
}

#[derive(Debug)]
pub struct Assignation<'a> {
    pub block_on: bool,
    pub var: String,
    pub to_assign: Statement<'a>,
}

#[derive(Debug)]
pub enum EExpression<'a> {
    Statement(Statement<'a>),
    Declaration(Assignation<'a>),
    Assignation(Assignation<'a>),
    // todo, unary/binary operation... idk
}
