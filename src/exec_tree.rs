use crate::scopes;
use std::collections::HashSet;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub struct Statement {
    pub inner: EStatement,
    /// external references that we can find in that statement.
    pub refs: HashSet<String>,
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

#[derive(Default, Debug, Clone)]
pub enum StdFunction {
    Atoi,
    Itoa,
    Printf,
    #[default]
    Nope,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub block_on: bool,
    pub params: Vec<Statement>,
    pub name: String,
    pub std: StdFunction,
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

impl From<scopes::Expression> for Expression {
    fn from(val: scopes::Expression) -> Self {
        Expression {
            inner: match val.inner {
                scopes::EExpression::Statement(n) => EExpression::Statement(n.into()),
                scopes::EExpression::Declaration(n) => EExpression::Declaration(n.into()),
                scopes::EExpression::Assignation(n) => EExpression::Assignation(n.into()),
            },
            latest: false,
        }
    }
}

impl From<scopes::Assignation> for Assignation {
    fn from(val: scopes::Assignation) -> Self {
        Assignation {
            block_on: val.block_on,
            var: format!(
                "{}#{}:{}:{}",
                val.info.scope, val.info.line, val.info.column, val.info.name
            ),
            to_assign: val.to_assign.into(),
        }
    }
}

impl From<scopes::Function> for Function {
    fn from(val: scopes::Function) -> Self {
        let form = |info: scopes::VarInfo| {
            format!("{}#{}:{}:{}", info.scope, info.line, info.column, info.name)
        };
        let args = val.args.into_iter().map(form).collect();
        let captures = val.inner.refs.iter().cloned().map(form).collect();
        Function {
            id: val.id,
            args,
            inner: val.inner.into(),
            captures,
            same_as: Default::default(),
        }
    }
}

impl From<scopes::Call> for Call {
    fn from(val: scopes::Call) -> Self {
        let n = val.name;
        Call {
            block_on: val.block_on,
            params: val.params.into_iter().map(|s| s.into()).collect(),
            name: format!("{}#{}:{}:{}", n.scope, n.line, n.column, n.name),
            std: StdFunction::default(),
        }
    }
}

impl From<scopes::Statement> for Statement {
    fn from(val: scopes::Statement) -> Self {
        Statement {
            inner: match val.inner {
                scopes::EStatement::Function(n) => EStatement::Function(n.into()),
                scopes::EStatement::Str(n) => EStatement::Str(n),
                scopes::EStatement::Num(n) => EStatement::Num(n),
                scopes::EStatement::Compound(n) => EStatement::Compound(n.into()),
                scopes::EStatement::Copy(n) => EStatement::Copy(n),
                scopes::EStatement::Ref(n) => {
                    EStatement::Ref(format!("{}#{}:{}:{}", n.scope, n.line, n.column, n.name))
                }
                scopes::EStatement::Call(n) => EStatement::Call(n.into()),
                scopes::EStatement::StdCall(n) => {
                    let stdf = match n.name.name.as_str() {
                        "atoi" => StdFunction::Atoi,
                        "itoa" => StdFunction::Itoa,
                        "printf" => StdFunction::Printf,
                        &_ => panic!("{} not found in this scope.", n.name.name),
                    };
                    let mut call: Call = n.into();
                    call.std = stdf;
                    EStatement::StdCall(call)
                }
                scopes::EStatement::Skip => unreachable!(),
            },
            refs: val
                .refs
                .into_iter()
                .map(|n| format!("{}#{}:{}:{}", n.scope, n.line, n.column, n.name))
                .collect(),
        }
    }
}

impl From<scopes::Compound> for Compound {
    fn from(val: scopes::Compound) -> Self {
        let mut inner: Vec<Expression> = val.inner.into_iter().map(|e| e.into()).collect();

        if !inner.is_empty() {
            let last = inner.len() - 1;
            inner[last].latest = true;
        }

        Compound {
            inner,
            block_on: val.block_on,
            decls: val
                .decls
                .into_iter()
                .map(|n| format!("{}#{}:{}:{}", n.scope, n.line, n.column, n.name))
                .collect(),
        }
    }
}
