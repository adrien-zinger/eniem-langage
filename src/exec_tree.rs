use crate::scopes;

#[derive(Debug, Clone)]
pub struct Statement {
    pub inner: EStatement,
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
    pub args: Vec<scopes::VarInfo>,
    pub inner: Compound,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub block_on: bool,
    pub params: Vec<Statement>,
    pub name: String,
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

impl Into<Expression> for scopes::Expression {
    fn into(self) -> Expression {
        Expression {
            inner: match self.inner {
                scopes::EExpression::Statement(n) => EExpression::Statement(n.into()),
                scopes::EExpression::Declaration(n) => EExpression::Declaration(n.into()),
                scopes::EExpression::Assignation(n) => EExpression::Assignation(n.into()),
            },
            latest: false,
        }
    }
}

impl Into<Assignation> for scopes::Assignation {
    fn into(self) -> Assignation {
        Assignation {
            block_on: self.block_on,
            var: self.var,
            to_assign: self.to_assign.into(),
        }
    }
}

impl Into<Function> for scopes::Function {
    fn into(self) -> Function {
        Function {
            args: self.args,
            inner: self.inner.into(),
        }
    }
}

impl Into<Call> for scopes::Call {
    fn into(self) -> Call {
        Call {
            block_on: self.block_on,
            params: self.params.into_iter().map(|s| s.into()).collect(),
            name: self.name,
        }
    }
}

impl Into<Statement> for scopes::Statement {
    fn into(self) -> Statement {
        Statement {
            inner: match self.inner {
                scopes::EStatement::Function(n) => EStatement::Function(n.into()),
                scopes::EStatement::Str(n) => EStatement::Str(n),
                scopes::EStatement::Compound(n) => EStatement::Compound(n.into()),
                scopes::EStatement::Copy(n) => EStatement::Copy(n),
                scopes::EStatement::Ref(n) => EStatement::Ref(n),
                scopes::EStatement::Call(n) => EStatement::Call(n.into()),
            },
        }
    }
}

impl Into<Compound> for scopes::Compound {
    fn into(self) -> Compound {
        let mut inner: Vec<Expression> = self.inner.into_iter().map(|e| e.into()).collect();

        let last = inner.len() - 1;
        inner[last].latest = true;
        Compound {
            inner,
            block_on: self.block_on,
        }
    }
}
