use crate::scopes;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub struct Statement {
    pub inner: EStatement,
    pub refs: Vec<String>,
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
    /// Id of the function, which is NOT the variable to access
    /// to it but a parsing Id to identify that subtree.
    pub id: String,
    pub args: Vec<String>,
    pub inner: Compound,
    /// Variables to capture.
    pub captures: Vec<String>,
    /// Calls tracking in abstract interpretation
    pub same_as: Arc<Mutex<Vec<Function>>>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl std::hash::Hash for Function {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Eq for Function {}

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
    /// Variable the compound declare.
    pub decls: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum EStatement {
    Function(Function),
    Str(String /* inner text */),
    Num(i32 /* inner signed number */),
    Compound(Compound),
    Copy(String /* variable name */),
    Ref(String),
    Call(Call),
    StdCall(Call),
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
            var: format!(
                "{}#{}:{}:{}",
                self.info.scope, self.info.line, self.info.column, self.info.name
            ),
            to_assign: self.to_assign.into(),
        }
    }
}

impl Into<Function> for scopes::Function {
    fn into(self) -> Function {
        let form = |info: scopes::VarInfo| {
            format!("{}#{}:{}:{}", info.scope, info.line, info.column, info.name)
        };
        let args = self.args.into_iter().map(form).collect();
        let captures = self.inner.refs.iter().cloned().map(form).collect();
        Function {
            id: self.id,
            args,
            inner: self.inner.into(),
            captures,
            same_as: Default::default(),
        }
    }
}

impl Into<Call> for scopes::Call {
    fn into(self) -> Call {
        let n = self.name;
        Call {
            block_on: self.block_on,
            params: self.params.into_iter().map(|s| s.into()).collect(),
            name: format!("{}#{}:{}:{}", n.scope, n.line, n.column, n.name),
        }
    }
}

impl Into<Statement> for scopes::Statement {
    fn into(self) -> Statement {
        Statement {
            inner: match self.inner {
                scopes::EStatement::Function(n) => EStatement::Function(n.into()),
                scopes::EStatement::Str(n) => EStatement::Str(n),
                scopes::EStatement::Num(n) => EStatement::Num(n),
                scopes::EStatement::Compound(n) => EStatement::Compound(n.into()),
                scopes::EStatement::Copy(n) => EStatement::Copy(n),
                scopes::EStatement::Ref(n) => {
                    EStatement::Ref(format!("{}#{}:{}:{}", n.scope, n.line, n.column, n.name))
                }
                scopes::EStatement::Call(n) => EStatement::Call(n.into()),
                scopes::EStatement::StdCall(n) => EStatement::StdCall(n.into()),
                scopes::EStatement::Skip => unreachable!(),
            },
            refs: self
                .refs
                .into_iter()
                .map(|n| format!("{}#{}:{}:{}", n.scope, n.line, n.column, n.name))
                .collect(),
        }
    }
}

impl Into<Compound> for scopes::Compound {
    fn into(self) -> Compound {
        let mut inner: Vec<Expression> = self.inner.into_iter().map(|e| e.into()).collect();

        if inner.len() > 0 {
            let last = inner.len() - 1;
            inner[last].latest = true;
        }

        Compound {
            inner,
            block_on: self.block_on,
            decls: self
                .decls
                .into_iter()
                .map(|n| format!("{}#{}:{}:{}", n.scope, n.line, n.column, n.name))
                .collect(),
        }
    }
}
