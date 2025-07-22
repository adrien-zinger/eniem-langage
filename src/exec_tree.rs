use crate::scopes;
use std::collections::{HashMap, HashSet};
use std::sync::{atomic::AtomicBool, Arc, Mutex};

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
    I32add,
    I32mult,
    I32notEqual,
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
    /// Contains module path if this object is a module.
    pub module: Option<String>,
    pub initialized: Arc<AtomicBool>,
    pub inner: Vec<Expression>,
    /// Variable the compound declare.
    pub decls: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum EStatement {
    Function(Function),
    Str(String /* inner text */),
    Num(i32 /* inner signed number */),
    Bool(bool),
    Compound(Arc<Compound>),
    Copy(String /* variable name */),
    Ref(String),
    Call(Call),
    StdCall(Call),
    Branch(Arc<Branch>),
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub condition: Statement,
    pub left: Statement,
    pub right: Statement,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub inner: EExpression,
    pub latest: bool,
}

#[derive(Debug, Clone)]
/// Assignation or declaration description.
pub struct Assignation {
    pub block_on: bool,
    /// Variable name. At this point, the name is unique.
    pub var: String,
    /// Right part of the assignation.
    pub to_assign: Statement,
    /// Field true if the variable to assign has to be
    /// replaced by the new variable pointer, or if the
    /// variable has to be written and modified.
    /// See parser: `parser::assignation`
    pub modify: bool,
}

#[derive(Debug, Clone)]
pub struct Using {
    pub var: String,
    pub module: Arc<Compound>,
}

#[derive(Debug, Clone)]
pub enum EExpression {
    Statement(Statement),
    Declaration(Assignation),
    Assignation(Assignation),
    Using(Using),
}

#[derive(Default)]
pub struct Scope2ETree {
    modules: HashMap<String, Arc<Compound>>,
}

impl Scope2ETree {
    pub fn expression(&mut self, val: scopes::Expression) -> Expression {
        Expression {
            inner: match val.inner {
                scopes::EExpression::Statement(n) => EExpression::Statement(self.statement(n)),
                scopes::EExpression::Declaration(n) => {
                    EExpression::Declaration(self.assignation(n))
                }
                scopes::EExpression::Assignation(n) => {
                    EExpression::Assignation(self.assignation(n))
                }
                scopes::EExpression::Using(n) => {
                    let n = n.borrow();
                    let module_id = n
                        .var
                        .as_ref()
                        .expect("variable info was expected here")
                        .scope
                        .module_path
                        .as_ref()
                        .expect("module path was expected here");
                    if let Some(module) = self.modules.get(module_id) {
                        EExpression::Using(Using {
                            var: n.name.clone(),
                            module: module.clone(),
                        })
                    } else {
                        let compound = self.compound(n.module.clone().unwrap().borrow().clone());
                        let module = Arc::new(compound);
                        self.modules.insert(module_id.clone(), module.clone());
                        EExpression::Using(Using {
                            var: n.name.clone(),
                            module: module.clone(),
                        })
                    }
                }
            },
            latest: false,
        }
    }

    fn assignation(&mut self, val: scopes::Assignation) -> Assignation {
        Assignation {
            block_on: val.block_on,
            var: format!(
                "{}#{}:{}:{}",
                val.info.scope.name, val.info.line, val.info.column, val.info.name
            ),
            to_assign: self.statement(val.to_assign),
            modify: val.modify,
        }
    }

    fn function(&mut self, val: scopes::Function) -> Function {
        let form = |info: scopes::VarInfo| {
            format!(
                "{}#{}:{}:{}",
                info.scope.name, info.line, info.column, info.name
            )
        };
        let args = val.args.into_iter().map(form).collect();
        let captures = val.inner.refs.iter().cloned().map(form).collect();
        Function {
            id: val.id,
            args,
            inner: self.compound(val.inner),
            captures,
            same_as: Default::default(),
        }
    }

    fn call(&mut self, val: scopes::Call) -> Call {
        let n = val.name;
        Call {
            block_on: val.block_on,
            params: val.params.into_iter().map(|s| self.statement(s)).collect(),
            name: format!("{}#{}:{}:{}", n.scope.name, n.line, n.column, n.name),
            std: StdFunction::default(),
        }
    }

    fn statement(&mut self, val: scopes::Statement) -> Statement {
        Statement {
            inner: match val.inner {
                scopes::EStatement::Branch(n) => EStatement::Branch(
                    Branch {
                        condition: self.statement(n.condition),
                        left: self.statement(n.left),
                        right: self.statement(n.right),
                    }
                    .into(),
                ),
                scopes::EStatement::Function(n) => EStatement::Function(self.function(n)),
                scopes::EStatement::Str(n) => EStatement::Str(n),
                scopes::EStatement::Num(n) => EStatement::Num(n),
                scopes::EStatement::Bool(n) => EStatement::Bool(n),
                scopes::EStatement::Compound(n) => {
                    if let Some(module_id) = &n.borrow().module {
                        if let Some(module) = self.modules.get(module_id) {
                            // Return a new reference of the module.
                            EStatement::Compound(module.clone())
                        } else {
                            // If the module is still not visited, add it to the known
                            // modules.
                            let module = Arc::new(self.compound(n.borrow().clone()));
                            self.modules.insert(module_id.clone(), module.clone());
                            EStatement::Compound(module)
                        }
                    } else {
                        // Not a module, just get it (cloning it) and put it into
                        // a Compound.
                        EStatement::Compound(Arc::new(self.compound(n.borrow().clone())))
                    }
                }
                scopes::EStatement::Copy(n) => EStatement::Copy(n),
                scopes::EStatement::Ref(n) => EStatement::Ref(format!(
                    "{}#{}:{}:{}",
                    n.scope.name, n.line, n.column, n.name
                )),
                scopes::EStatement::Call(n) => EStatement::Call(self.call(n)),
                scopes::EStatement::StdCall(n) => {
                    let stdf = match n.name.name.as_str() {
                        "atoi" => StdFunction::Atoi,
                        "itoa" => StdFunction::Itoa,
                        "printf" => StdFunction::Printf,
                        "i32_mult" => StdFunction::I32mult,
                        "i32_add" => StdFunction::I32add,
                        "i32_not_equal" => StdFunction::I32notEqual,
                        &_ => panic!("{} not found in this scope.", n.name.name),
                    };
                    let mut call = self.call(n);
                    call.std = stdf;
                    EStatement::StdCall(call)
                }
                scopes::EStatement::Skip => unreachable!(),
            },
            refs: val
                .refs
                .into_iter()
                .map(|n| format!("{}#{}:{}:{}", n.scope.name, n.line, n.column, n.name))
                .collect(),
        }
    }

    pub fn compound(&mut self, val: scopes::Compound) -> Compound {
        let mut inner: Vec<Expression> =
            val.inner.into_iter().map(|e| self.expression(e)).collect();

        if !inner.is_empty() {
            let last = inner.len() - 1;
            inner[last].latest = true;
        }

        Compound {
            inner,
            block_on: val.block_on,
            module: val.module,
            initialized: Arc::new(AtomicBool::new(false)),
            decls: val
                .decls
                .into_iter()
                .map(|n| format!("{}#{}:{}:{}", n.scope.name, n.line, n.column, n.name))
                .collect(),
        }
    }
}
