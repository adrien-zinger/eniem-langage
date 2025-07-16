use crate::tree;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_scopes")]
        std::println!($($rest)*)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Scope {
    pub name: String,
    pub module_path: Option<String>,
}

impl Scope {
    fn push(&self, line: u32, column: usize) -> Self {
        Scope {
            name: format!("{}:scope_{}:{}", self.name, line, column),
            module_path: None,
        }
    }

    fn push_mod(&self, module: String) -> Self {
        let module_path = if let Some(module_path) = &self.module_path {
            Some(format!("{module_path}::{module}"))
        } else {
            Some(module.clone())
        };
        Scope {
            name: format!("{}:module_{}", self.name, module),
            module_path,
        }
    }

    fn push_fence(&self) -> Self {
        Scope {
            name: format!("{}&", self.name),
            module_path: self.module_path.clone(),
        }
    }

    fn is_module(&self) -> bool {
        self.module_path.is_some()
    }

    fn module(&self) -> String {
        if let Some(m) = &self.module_path {
            m.clone()
        } else {
            panic!("unexpected module unwrap");
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct VarInfo {
    pub name: String,
    pub line: u32,
    pub column: usize,
    pub scope: Scope,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub inner: EStatement,
    /// external refs
    pub refs: HashSet<VarInfo>,
    pub line: u32,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: String,
    pub args: Vec<VarInfo>,
    pub inner: Compound,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub block_on: bool,
    pub params: Vec<Statement>,
    pub name: VarInfo,
}

#[derive(Default, Debug, Clone)]
pub struct Compound {
    pub block_on: bool,
    pub inner: Vec<Expression>,
    pub decls: Vec<VarInfo>,
    /// external refs
    pub refs: HashSet<VarInfo>,
    pub module: Option<String>,
}

#[derive(Debug, Clone)]
pub enum EStatement {
    Function(Function),
    Str(String /* inner text */),
    Num(i32 /* inner signed number */),
    Compound(RefCell<Compound>),
    Copy(String /* variable name */),
    Ref(VarInfo),
    Call(Call),
    StdCall(Call),
    Skip,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub inner: EExpression,
}

#[derive(Debug, Clone)]
pub struct Assignation {
    pub block_on: bool,
    pub var: String,
    pub info: VarInfo,
    pub to_assign: Statement,
    pub is_ref: Option<VarInfo>,
    pub modify: bool,
}

#[derive(Debug, Clone)]
pub struct Using {
    pub name: String,
    pub var: Option<VarInfo>,
    pub module: Option<RefCell<Compound>>,
}

#[derive(Debug, Clone)]
pub enum EExpression {
    Statement(Statement),
    Declaration(Assignation),
    Assignation(Assignation),
    /// Using expression `use my_module::foo`
    // This is a refcell because we share the reference between
    // the tree and the `usings` vector to process the nodes after
    // all.
    Using(RefCell<Using>),
}

/// Look into declaration list for a valid variable.
fn lookup(var: &str, decls: &[VarInfo]) -> Option<VarInfo> {
    for decl in decls.iter().rev() {
        if decl.name == var {
            return Some(decl.clone());
        }
    }
    None
}

fn extend_refs(refs: &mut HashSet<VarInfo>, other: &HashSet<VarInfo>, current_scope: &Scope) {
    refs.extend(
        other
            .iter()
            .filter(|v| v.scope.name != current_scope.name)
            .cloned(),
    );
}

#[derive(Debug, Default)]
/// Separated pass to check code validity.
pub struct Scopes {
    pub errors: Vec<String>,

    /// Variables that are declared in modules.
    pub static_decls: HashSet<VarInfo>,

    /// Modules
    pub modules: HashMap<String, RefCell<Compound>>,

    /// Using references
    pub usings: Vec<RefCell<Using>>,
}

impl Scopes {
    /// Handle an assignation
    fn assignation(
        &mut self,
        assign: tree::Assignation,
        info: VarInfo,
        scope: Scope,
        decls: Vec<VarInfo>,
    ) -> Assignation {
        let block_on = assign.block_on;
        let var = assign.var.clone();
        let is_ref = if let Some(info) = lookup(&var, &decls) {
            if info.scope != scope {
                Some(info)
            } else {
                None
            }
        } else {
            self.errors
                .push(format!("{var} not declared in this scope."));
            None
        };
        let to_assign = self.statement(assign.to_assign, scope, decls);
        Assignation {
            info,
            block_on,
            var,
            to_assign,
            is_ref,
            modify: assign.modify,
        }
    }

    /// Handle a statement
    fn statement(
        &mut self,
        input: tree::Statement,
        scope: Scope,
        decls: Vec<VarInfo>,
    ) -> Statement {
        let line = input.pos.location_line();
        let column = input.pos.get_column();
        let mut refs = HashSet::new();
        let inner = match input.inner {
            tree::EStatement::Function(f) => {
                let new_scope = scope.push(line, column);
                // todo: modify parser in order to have line and column for each parameter.
                let args: Vec<VarInfo> = f
                    .args
                    .into_iter()
                    .map(|arg| VarInfo {
                        name: arg,
                        line,
                        column,
                        scope: new_scope.clone(),
                    })
                    .collect();
                let mut decls = decls.clone();
                decls.append(&mut args.clone());
                let inner = self.compound(f.inner, new_scope.clone(), decls);
                extend_refs(&mut refs, &inner.refs, &scope);
                EStatement::Function(Function {
                    id: new_scope.name,
                    inner,
                    args,
                })
            }
            tree::EStatement::Str(text) => EStatement::Str(text),
            tree::EStatement::Num(num) => EStatement::Num(num),
            tree::EStatement::Operation(op) => self.operation(*op, &mut refs, scope, decls),
            tree::EStatement::Compound(c) => {
                let new_scope = scope.push(line, column);
                let compound = self.compound(c, new_scope.clone(), decls.clone());
                extend_refs(&mut refs, &compound.refs, &new_scope);
                debug!("compound refs merged: {:?}", refs);
                EStatement::Compound(RefCell::new(compound))
            }
            tree::EStatement::Copy(v) => {
                if let Some(info) = lookup(&v, &decls) {
                    if info.scope != scope {
                        refs.insert(info);
                    }
                } else {
                    self.errors
                        .push(format!("{} not declared in this scope.", v));
                }
                EStatement::Copy(v)
            }
            tree::EStatement::Ref(v) => {
                if let Some(info) = lookup(&v, &decls) {
                    if info.scope != scope {
                        refs.insert(info.clone());
                    }
                    EStatement::Ref(info)
                } else {
                    self.errors
                        .push(format!("{} not declared in this scope.", v));
                    EStatement::Skip
                }
            }
            tree::EStatement::Call(c) => {
                let mut params = vec![];
                for param in c.params {
                    let param = self.statement(param, scope.clone(), decls.clone());
                    extend_refs(&mut refs, &param.refs, &scope);
                    params.push(param);
                }
                if let Some(info) = lookup(&c.name, &decls) {
                    if info.scope != scope {
                        refs.insert(info.clone());
                    }
                    EStatement::Call(Call {
                        block_on: c.block_on,
                        params,
                        name: info,
                    })
                } else {
                    self.errors
                        .push(format!("{} not declared in this scope.", c.name));
                    EStatement::Skip
                }
            }
            tree::EStatement::StdCall(c) => {
                if c.name == "printf"
                    || c.name == "atoi"
                    || c.name == "i32_add"
                    || c.name == "i32_mult"
                {
                } else {
                    panic!("unknown std function {}", c.name);
                }
                let mut params = vec![];
                for param in c.params {
                    let param = self.statement(param, scope.clone(), decls.clone());
                    extend_refs(&mut refs, &param.refs, &scope);
                    params.push(param);
                }
                EStatement::StdCall(Call {
                    block_on: c.block_on,
                    params,
                    name: VarInfo {
                        name: c.name.clone(),
                        line,
                        column,
                        scope,
                    },
                })
            }
        };
        Statement {
            inner,
            line,
            column,
            refs,
        }
    }

    /// Translate an operation into a call statement.
    fn operation(
        &mut self,
        operation: tree::Operation,
        refs: &mut HashSet<VarInfo>,
        scope: Scope,
        decls: Vec<VarInfo>,
    ) -> EStatement {
        /// Get function name from parsed operator.
        fn get_name(operator: &tree::Operator) -> &'static str {
            match operator {
                tree::Operator::EqualEqual => "equal",
                tree::Operator::NotEqual => "not_equal",
                tree::Operator::Minus => "minus",
                tree::Operator::Plus => "plus",
                _ => todo!(),
            }
        }

        // Transform an operation into a call statement to execute.
        // An operation can be Unary: contains a single statement
        // Or binary: contains two Operation.
        let (name, params) = match operation {
            tree::Operation::Unary(operation) => {
                // Set function parameter
                let param = self.statement(operation.statement, scope.clone(), decls.clone());
                extend_refs(refs, &param.refs, &scope);
                (get_name(&operation.operator), vec![param])
            }
            tree::Operation::Binary(operation) => {
                // Set function parameters - recursive call
                let left = self.operation(operation.left, refs, scope.clone(), decls.clone());
                let right = self.operation(operation.right, refs, scope.clone(), decls.clone());
                let left = Statement {
                    inner: left,
                    line: 0,
                    refs: refs.clone(),
                    column: 0,
                };
                let right = Statement {
                    inner: right,
                    line: 0,
                    refs: refs.clone(),
                    column: 0,
                };
                (get_name(&operation.operator), vec![left, right])
            }
        };

        // Find function name in the current scope. Insert it
        // as a reference in the current scope if it's in an upper
        // scope. Return a call statement.
        if let Some(info) = lookup(name, &decls) {
            if info.scope != scope {
                refs.insert(info.clone());
            }
            EStatement::Call(Call {
                block_on: false,
                params,
                name: info,
            })
        } else {
            self.errors
                .push(format!("{} not declared in this scope.", name));
            EStatement::Skip
        }
    }

    fn compound(
        &mut self,
        mut input: tree::Compound,
        mut scope: Scope,
        mut decls: Vec<VarInfo>,
    ) -> Compound {
        let mut inner = vec![];
        let mut refs = HashSet::new();
        let mut local_decls = vec![];
        #[cfg(feature = "debug_scopes")]
        let len = input.inner.len();
        loop {
            let mut end = 0;
            // Add all declarations of the scope.
            if scope.is_module() {
                for expr in &input.inner {
                    if let tree::EExpression::Declaration(a) = &expr.inner {
                        let v = VarInfo {
                            name: format!("{}::{}", scope.module(), a.var.clone()),
                            scope: scope.clone(),
                            line: expr.pos.location_line(),
                            column: expr.pos.get_column(),
                        };
                        self.static_decls.insert(v);
                    }
                    if expr.is_blocking() {
                        panic!("a module cannot be blocking")
                    }
                }
            }

            for expr in &input.inner {
                end += 1;
                if let tree::EExpression::Declaration(a) = &expr.inner {
                    let v = VarInfo {
                        name: a.var.clone(),
                        scope: scope.clone(),
                        line: expr.pos.location_line(),
                        column: expr.pos.get_column(),
                    };
                    decls.push(v.clone());
                    local_decls.push(v)
                }
                if expr.is_blocking() {
                    debug!("break cause {end} is blocking");
                    break;
                }
            }

            for expr in input.inner.drain(..end) {
                inner.push(match expr.inner {
                    tree::EExpression::Declaration(a) => {
                        let info = VarInfo {
                            name: a.var.clone(),
                            scope: scope.clone(),
                            line: expr.pos.location_line(),
                            column: expr.pos.get_column(),
                        };
                        let a = self.assignation(a, info, scope.clone(), decls.clone());
                        extend_refs(&mut refs, &a.to_assign.refs, &scope);
                        EExpression::Declaration(a)
                    }
                    tree::EExpression::Assignation(a) => {
                        if let Some(info) = lookup(&a.var, &decls) {
                            debug!("detect assign to {}", a.var);
                            let a = self.assignation(a, info, scope.clone(), decls.clone());
                            extend_refs(&mut refs, &a.to_assign.refs, &scope);
                            if let Some(info) = &a.is_ref {
                                extend_refs(&mut refs, &HashSet::from_iter([info.clone()]), &scope);
                            }
                            EExpression::Assignation(a)
                        } else {
                            self.errors.push("variable not found".to_string());
                            continue;
                        }
                    }
                    tree::EExpression::Statement(s) => {
                        let s = self.statement(s, scope.clone(), decls.clone());
                        extend_refs(&mut refs, &s.refs, &scope);
                        EExpression::Statement(s)
                    }
                    tree::EExpression::Module(m) => {
                        let line = expr.pos.location_line();
                        let column = expr.pos.get_column();
                        let new_scope = scope.push_mod(m.name);
                        let compound =
                            RefCell::new(self.compound(m.inner, new_scope.clone(), decls.clone()));
                        extend_refs(&mut refs, &compound.borrow().refs, &new_scope);
                        debug!("compound refs merged: {:?}", refs);
                        self.modules
                            .insert(scope.module_path.clone().unwrap(), compound.clone());
                        let refs = compound.borrow().refs.clone();
                        EExpression::Statement(Statement {
                            line,
                            refs,
                            column,
                            inner: EStatement::Compound(compound),
                        })
                    }
                    tree::EExpression::Using(name) => {
                        // Resolved later
                        let using = RefCell::new(Using {
                            name,
                            var: None,
                            module: None,
                        });
                        self.usings.push(using.clone());
                        EExpression::Using(using)
                    }
                })
            }
            if input.inner.is_empty() {
                debug!("break compound");
                break;
            } else {
                debug!("end: {end} != len: {len}");
                scope = scope.push_fence();
            }
        }
        Compound {
            inner: inner
                .into_iter()
                .map(|inner| Expression { inner })
                .collect(),
            block_on: input.block_on,
            refs,
            decls: local_decls,
            module: scope.module_path,
        }
    }

    /// Start running Scope pass.
    ///
    /// This pass is required by the interpreter. It'll check that the user
    /// correctly declare and use variables in his project accordingly to
    /// the interpretation requirement. It also rename variables with a single
    /// id derived from his position and his code block.
    pub fn check(&mut self, input: Vec<tree::Expression>) -> Vec<Expression> {
        let ret = self
            .compound(
                tree::Compound {
                    inner: input,
                    block_on: false,
                },
                Scope {
                    name: "main".to_string(),
                    module_path: None, /* todo: is main a module? */
                },
                vec![],
            )
            .inner;

        for mut using in self.usings.iter().map(|u| u.borrow_mut()) {
            let info = self.static_decls.iter().find(|d| d.name == using.name);
            if info.is_some() {
                using.var = info.cloned();
                let info = info.as_ref().unwrap();
                let module_id = info.scope.module_path.as_ref().unwrap();
                using.module = self.modules.get(module_id).cloned();
            } else {
                self.errors
                    .push(format!("Reference to {} not found", using.name));
            }
        }

        ret
    }
}
