use crate::tree;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_scopes")]
        std::println!($($rest)*)
    }
}

#[derive(Clone, Debug)]
pub struct VarInfo {
    pub name: String,
    pub line: u32,
    pub column: usize,
    pub scope: String,
}

pub struct Statement {
    pub inner: EStatement,
    /// external refs
    pub refs: Vec<VarInfo>,
    pub line: u32,
    pub column: usize,
}

pub struct Function {
    pub id: String,
    pub args: Vec<VarInfo>,
    pub inner: Compound,
}

pub struct Call {
    pub block_on: bool,
    pub params: Vec<Statement>,
    pub name: VarInfo,
}

pub struct Compound {
    pub block_on: bool,
    pub inner: Vec<Expression>,
    pub decls: Vec<VarInfo>,
    /// external refs
    pub refs: Vec<VarInfo>,
}

pub enum EStatement {
    Function(Function),
    Str(String /* inner text */),
    Num(i32 /* inner signed number */),
    Compound(Compound),
    Copy(String /* variable name */),
    Ref(VarInfo),
    Call(Call),
    StdCall(Call),
    Skip,
}

pub struct Expression {
    pub inner: EExpression,
}

pub struct Assignation {
    pub block_on: bool,
    pub var: String,
    pub info: VarInfo,
    pub to_assign: Statement,
}

pub enum EExpression {
    Statement(Statement),
    Declaration(Assignation),
    Assignation(Assignation),
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

pub struct Scopes {
    pub errors: Vec<String>,
}

impl Scopes {
    fn assignation(
        &mut self,
        assign: tree::Assignation,
        info: VarInfo,
        scope: String,
        decls: Vec<VarInfo>,
    ) -> Assignation {
        let block_on = assign.block_on;
        let var = assign.var.clone();
        let to_assign = self.statement(assign.to_assign, scope, decls);
        Assignation {
            info,
            block_on,
            var,
            to_assign,
        }
    }

    fn statement(
        &mut self,
        input: tree::Statement,
        scope: String,
        decls: Vec<VarInfo>,
    ) -> Statement {
        let line = input.pos.location_line();
        let column = input.pos.get_column();
        let mut refs = vec![];
        let inner = match input.inner {
            tree::EStatement::Function(f) => {
                let new_scope = format!("{}:block_{}_{}", scope, line, column);
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
                refs.append(&mut inner.refs.clone());
                EStatement::Function(Function {
                    id: new_scope,
                    inner,
                    args,
                })
            }
            tree::EStatement::Str(text) => EStatement::Str(text),
            tree::EStatement::Num(num) => EStatement::Num(num),
            tree::EStatement::Operation(_) => todo!(),
            tree::EStatement::Compound(c) => {
                let new_scope = format!("{}:block_{}_{}", scope, line, column);
                EStatement::Compound(self.compound(c, new_scope, decls.clone()))
            }
            tree::EStatement::Copy(v) => {
                if let Some(info) = lookup(&v, &decls) {
                    if info.scope != scope {
                        refs.push(info);
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
                        refs.push(info.clone());
                    }
                    EStatement::Ref(info)
                } else {
                    self.errors
                        .push(format!("{} not declared in this scope.", v));
                    EStatement::Skip
                }
            }
            tree::EStatement::Call(c) => {
                if let Some(info) = lookup(&c.name, &decls) {
                    if info.scope != scope {
                        refs.push(info);
                    }
                } else {
                    self.errors
                        .push(format!("{} not declared in this scope.", c.name));
                }
                let mut params = vec![];
                for param in c.params {
                    let param = self.statement(param, scope.clone(), decls.clone());
                    refs.append(&mut param.refs.clone());
                    params.push(param);
                }
                if let Some(info) = lookup(&c.name, &decls) {
                    if info.scope != scope {
                        refs.push(info.clone());
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
                if c.name == "printf" || c.name == "atoi" || c.name == "itoa" {
                } else {
                    panic!("unknown std function {}", c.name);
                }
                let mut params = vec![];
                for param in c.params {
                    let param = self.statement(param, scope.clone(), decls.clone());
                    refs.append(&mut param.refs.clone());
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

    fn compound(
        &mut self,
        mut input: tree::Compound,
        mut scope: String,
        mut decls: Vec<VarInfo>,
    ) -> Compound {
        debug!("Check {scope} variable's scope");
        let mut inner = vec![];
        let mut refs = vec![];
        let mut local_decls = vec![];
        #[cfg(feature = "debug_scopes")]
        let len = input.inner.len();
        let mut scope_pos = 0;
        loop {
            let mut end = 0;
            // Add all declarations of the scope.
            for expr in &input.inner {
                debug!("start at {end}");
                end += 1;
                if let tree::EExpression::Declaration(a) = &expr.inner {
                    debug!("{} declared in scope {scope}", a.var);
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
                        let mut arefs = a.to_assign.refs.clone();
                        refs.append(&mut arefs);
                        EExpression::Declaration(a)
                    }
                    tree::EExpression::Assignation(a) => {
                        if let Some(info) = lookup(&a.var, &decls) {
                            debug!("detect assign to {}", a.var);
                            let a = self.assignation(a, info, scope.clone(), decls.clone());
                            refs.append(&mut a.to_assign.refs.clone());
                            EExpression::Assignation(a)
                        } else {
                            self.errors.push("variable not found".to_string());
                            continue;
                        }
                    }
                    tree::EExpression::Statement(s) => {
                        let s = self.statement(s, scope.clone(), decls.clone());
                        refs.append(&mut s.refs.clone());
                        EExpression::Statement(s)
                    }
                })
            }
            if input.inner.is_empty() {
                debug!("break compound");
                break;
            } else {
                debug!("end: {end} != len: {len}");
                scope = format!("{}_", scope_pos);
                scope_pos += 1;
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
        }
    }

    /// Start running Scope pass.
    ///
    /// This pass is required by the interpreter. It'll check that the user
    /// correctly declare and use variables in his project accordingly to
    /// the interpretation requirement. It also rename variables with a single
    /// id derived from his position and his code block.
    pub fn check(&mut self, input: Vec<tree::Expression>) -> Vec<Expression> {
        self.compound(
            tree::Compound {
                inner: input,
                block_on: false,
            },
            "main".to_string(),
            vec![],
        )
        .inner
    }
}
