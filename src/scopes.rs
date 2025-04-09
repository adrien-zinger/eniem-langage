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
    Compound(Compound),
    Copy(String /* variable name */),
    Ref(VarInfo),
    Call(Call),
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
    // todo, unary/binary operation... idk
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
                        line: line,
                        column: column,
                        scope: new_scope.clone(),
                    })
                    .collect();
                let mut decls = decls.clone();
                decls.append(&mut args.clone());
                let inner = self.compound(f.inner, new_scope, decls);
                refs.append(&mut inner.refs.clone());
                EStatement::Function(Function { inner, args })
            }
            tree::EStatement::Str(text) => EStatement::Str(text),
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
        input: tree::Compound,
        scope: String,
        mut decls: Vec<VarInfo>,
    ) -> Compound {
        // Add all declarations of the scope.
        let mut local_decls = vec![];
        for expr in &input.inner {
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
        }

        let mut inner = vec![];
        let mut refs = vec![];
        for expr in input.inner {
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
                        self.errors.push(format!("variable not found"));
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

    pub fn check(&mut self, input: Vec<tree::Expression>) -> Vec<Expression> {
        let mut decls = vec![];
        let scope = "main".to_string();
        // Add all declarations of the scope.
        for expr in &input {
            if let tree::EExpression::Declaration(a) = &expr.inner {
                let v = VarInfo {
                    name: a.var.clone(),
                    scope: scope.clone(),
                    line: expr.pos.location_line(),
                    column: expr.pos.get_column(),
                };
                decls.push(v);
            }
        }

        let mut ret = vec![];
        for expr in input {
            ret.push(match expr.inner {
                tree::EExpression::Declaration(a) => {
                    let info = VarInfo {
                        name: a.var.clone(),
                        scope: scope.clone(),
                        line: expr.pos.location_line(),
                        column: expr.pos.get_column(),
                    };
                    let a = self.assignation(a, info, scope.clone(), decls.clone());
                    EExpression::Declaration(a)
                }
                tree::EExpression::Assignation(a) => {
                    debug!("detect assign to {}", a.var);
                    if let Some(info) = lookup(&a.var, &decls) {
                        let a = self.assignation(a, info, scope.clone(), decls.clone());
                        EExpression::Assignation(a)
                    } else {
                        debug!("detect assign error to {}", a.var);
                        self.errors
                            .push(format!("{} not declared in this scope", a.var));
                        continue;
                    }
                }
                tree::EExpression::Statement(s) => {
                    let s = self.statement(s, scope.clone(), decls.clone());
                    EExpression::Statement(s)
                }
            })
        }
        ret.into_iter().map(|inner| Expression { inner }).collect()
    }
}
