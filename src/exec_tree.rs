use crate::tree;

#[derive(Debug, Clone)]
pub struct Statement {
    pub inner: EStatement,
	pub is_last: bool,
}

impl Statement {
	pub fn is_blocking(&self) -> bool {
		match &self.inner {
			EStatement::Compound(n) => n.block_on,
			EStatement::Call(n) => n.block_on,
			_ => false,
		}
	}
}

#[derive(Debug, Clone)]
pub struct Function {
    pub args: Vec<String>,
    pub inner: Compound,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub block_on: bool,
    pub params: Vec<Statement>,
	pub name: String
}

#[derive(Debug, Clone)]
pub struct Compound {
    pub block_on: bool,
    pub inner: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum EStatement {
    Function(Function),
    Str(String /* inner text */),
    Compound(Compound),
    Copy(String /* variable name */),
    Ref(String /* variable name */),
    Call(Call),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub inner: EExpression,
	pub latest: bool,
}

#[derive(Debug, Clone)]
pub struct Assignation {
    pub block_on: bool,
    pub var: String,
    pub to_assign: Statement,
}

#[derive(Debug, Clone)]
pub enum EExpression {
    Statement(Statement),
    Declaration(Assignation),
    Assignation(Assignation),
    // todo, unary/binary operation... idk
}

impl Into<Expression> for tree::Expression<'_> {
	fn into(self) -> Expression {
		Expression {
			inner: match self.inner {
				tree::EExpression::Statement(n) => EExpression::Statement(n.into()),
				tree::EExpression::Declaration(n) => EExpression::Declaration(n.into()),
				tree::EExpression::Assignation(n) => EExpression::Assignation(n.into()),
			},
			latest: false,
		}
	}
}

impl Into<Assignation> for tree::Assignation<'_> {
	fn into(self) -> Assignation {
		Assignation {
			block_on: self.block_on,
			var: self.var,
			to_assign: self.to_assign.into()
		}
	}
}

impl Into<Function> for tree::Function<'_> {
	fn into(self) -> Function {
		Function {
			args: self.args,
			inner: self.inner.into()
		}
	}
}

impl Into<Call> for tree::Call<'_> {
	fn into(self) -> Call {
		Call {
			block_on: self.block_on,
			params: self.params.into_iter().map(|s| s.into()).collect(),
			name: self.name
		}
	}
}

impl Into<Statement> for tree::Statement<'_> {
	fn into(self) -> Statement {
		Statement {
			inner: match self.inner {
	 		   	tree::EStatement::Function(n) => EStatement::Function(n.into()),
    	   		tree::EStatement::Str(n) => EStatement::Str(n),
    	   		tree::EStatement::Compound(n) => EStatement::Compound(n.into()),
    	   		tree::EStatement::Copy(n) => EStatement::Copy(n),
    	   		tree::EStatement::Ref(n) => EStatement::Ref(n),
    	   		tree::EStatement::Call(n) => EStatement::Call(n.into()),
			},
			is_last: false
		}
	}
}

impl Into<Compound> for tree::Compound<'_> {
	fn into(self) -> Compound {
		let mut inner: Vec<Expression> = self.inner.into_iter().map(|e| e.into()).collect();

		let last = inner.len() - 1;
		inner[last].latest = true;
		Compound {
			inner,
			block_on: self.block_on
		}
	}
}
