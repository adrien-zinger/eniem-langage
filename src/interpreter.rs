use crate::exec_tree::*;

use std::collections::HashMap;
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

#[derive(Debug)]
enum Variable {
    /// Function tree and a list of captured variables.
    Function(Mutex<(Function, Vec<(String, Arc<Variable>)>)>),
    /// A mutable String
    String(Mutex<String>),
    /// Nothing, also default variable. Usually it's
    /// used as the default value of a scope.
    Empty,
}

impl Default for Variable {
    fn default() -> Self {
        Self::Empty
    }
}

type BoxVariable = Arc<AtomicPtr<Arc<Variable>>>;

#[derive(Clone)]
enum EJob {
    Expressions(Vec<Expression>),
    Expression(Expression),
    /// Write value from box into memory (end scope)
    Write((String, BoxVariable, Vec<String> /* variables declared in the scope */)),
    /// Free scope
    Delete(Vec<String>),
    /// End of scope
    Empty(Vec<String>),
}

#[derive(Clone)]
struct Job {
    inner: EJob,
    next: Option<EJob>,
    scope: Arc<Scope>,
}

struct Scope {
    id: u64,
    len: AtomicU64,
    value: BoxVariable,
    /// Parent job
    job: Option<Job>,
}

#[derive(Default)]
pub struct Interpreter {
    jobs: Arc<Mutex<Vec<Job>>>,
    counter: AtomicU64,
    variables: Arc<Mutex<HashMap<String, Arc<Variable>>>>,
}

impl Interpreter {
    fn new_id(&self) -> u64 {
        self.counter.fetch_add(1, Ordering::SeqCst)
    }

    /// Look for a variable in memory. The variable is forced to be in
    /// the current scope job or in a upper scope. If there is no variable
    /// found, it means that the variable is still not initialized.
    fn find(&self, var: &str, job: &Job) -> Option<Arc<Variable>> {
        let mut scope = &job.scope;
        loop {
            let key = format!("{}::{}", var, scope.id);
            debug!("look at: {}", key);
            if let Ok(vars) = self.variables.lock() {
                let varbox = vars.get(&key);
                if varbox.is_some() {
                    return varbox.cloned();
                }
            }

            if let Some(job) = &scope.job {
                scope = &job.scope;
            } else {
                return None;
            }
        }
    }

    /// public access to interpreter
    pub fn run(&self, input: &[Expression]) {
        debug!("start interpreter");

        self.expressions(
            input,
            Arc::new(Scope {
                id: self.new_id(),
                len: Default::default(),
                value: Default::default(),
                job: None,
            }),
        );

        loop {
            let job = if let Ok(jobs) = &mut self.jobs.lock() {
                if let Some(job) = jobs.pop() {
                    job.clone()
                } else {
                    return;
                }
            } else {
                continue;
            };
            self.exec(job);
        }
    }

    fn schedule(&self, job: Job) {
        if let Ok(jobs) = &mut self.jobs.lock() {
            jobs.push(job);
        }
    }

    fn schedule_later(&self, job: Job) {
        if let Ok(jobs) = &mut self.jobs.lock() {
            jobs.insert(0, job);
        }
    }

    /// Create jobs for a list of expressions. Doesn't execute things
    /// as the exec function, assignation or call_statement. Just scheduling
    /// jobs.
    fn expressions(&self, exprs: &[Expression], scope: Arc<Scope>) {
        let mut index = 0;
        for expr in exprs {
            index = index + 1;
            let blocking;
            let mut job = match &expr.inner {
                EExpression::Statement(input) => {
                    blocking = input.is_blocking();
                    Job {
                        inner: EJob::Expression(expr.clone()),
                        next: None,
                        scope: scope.clone(),
                    }
                }
                EExpression::Assignation(input) => {
                    blocking = input.block_on;
                    Job {
                        inner: EJob::Expression(expr.clone()),
                        next: None,
                        scope: scope.clone(),
                    }
                }
                EExpression::Declaration(input) => {
                    blocking = input.block_on;
                    Job {
                        inner: EJob::Expression(expr.clone()),
                        next: None,
                        scope: scope.clone(),
                    }
                }
            };

            if blocking {
                job.next = Some(EJob::Expressions(exprs[index..].to_vec()));
                self.schedule(job);
                break;
            } else {
                self.schedule(job);
            }
        }
    }

    fn assignation(&self, assign: &Assignation, job: Job) {
        match &assign.to_assign.inner {
            EStatement::Compound(input) => {
                debug!("assignation create a scope");
                let value = BoxVariable::default();
                debug!("scope decls: {:?}", input.decls);
				let new_scope_id = self.new_id();
				let decls = input
                    .decls
                    .iter()
                    .map(|id| format!("{}::{}", id, new_scope_id))
                    .collect();

                let job = Job {
                    inner: EJob::Write((
                        format!("{}::{}", assign.var, job.scope.id),
                        value.clone(),
						decls,
                    )),
                    scope: job.scope,
                    next: job.next,
                };
                let scope = Arc::new(Scope {
                    id: new_scope_id,
                    len: AtomicU64::new(input.inner.len() as u64),
                    value,
                    job: Some(job),
                });
                self.expressions(&input.inner, scope);
            }
            EStatement::Str(val) => {
                if let Ok(vars) = &mut self.variables.lock() {
                    let id = format!("{}::{}", assign.var, job.scope.id);
                    debug!("Write {} into {}", val, id);
                    vars.insert(id, Arc::new(Variable::String(Mutex::new(val.clone()))));
                }
                self.complete_job(job);
            }
            EStatement::Function(f) => {
                debug!("Declare a function");
                if let Ok(vars) = &mut self.variables.lock() {
                    let id = format!("{}::{}", assign.var, job.scope.id);
                    debug!("Write {:?} into {}", f, id);
                    let mut captures = vec![];
                    for c in &f.captures {
                        if let Some(var) = self.find(c, &job) {
                            captures.push((c.clone(), var));
                        } else {
                            self.schedule_later(job);
                            return;
                        }
                    }
                    vars.insert(
                        id,
                        Arc::new(Variable::Function(Mutex::new((f.clone(), captures)))),
                    );
                }
                self.complete_job(job);
            }
            EStatement::Call(c) => {
                let id = format!("{}::{}", assign.var, job.scope.id);
                self.call_statement(c, job, false, Some(id))
            }
            EStatement::Copy(_c) => todo!(),
            EStatement::Ref(c) => {
                debug!("try to assign {} from {c}", assign.var);
                let r = self.find(c, &job);
                if r.is_none() {
                    self.schedule_later(job);
                    return;
                }
                let val = r.unwrap();
                if let Ok(vars) = &mut self.variables.lock() {
                    let id = format!("{}::{}", assign.var, job.scope.id);
                    debug!("assign {} from {c}! {:?}", assign.var, val);
                    vars.insert(id, val);
                }
                self.complete_job(job);
            }
        }
    }

    /// Decrement scope len because we complete a Job and call nexts jobs.
    fn complete_job(&self, job: Job) {
        if job.scope.len.fetch_sub(1, Ordering::SeqCst) == 1 {
            self.schedule(job.scope.job.clone().unwrap());
        }
        if let Some(next) = job.next {
            if let EJob::Expressions(exprs) = next {
                self.expressions(&exprs, job.scope);
            }
        }
    }

    /// Execute a call statement.
    ///  call: reference to the call statement.
    ///  job: copy of the full job containing the statement.
    ///  latest: is it the latest expression of the current scope.
    ///  write: Some if the result has to be assigned to something, otherwise None.
    fn call_statement(&self, call: &Call, job: Job, latest: bool, write: Option<String>) {
        // find function in memory
        let function = if let Some(function) = self.find(&call.name, &job) {
            function
        } else {
            debug!("function not found");
            self.schedule_later(job);
            return;
        };

        let (function, captures) = if let Variable::Function(function) = &*function {
            if let Ok(function) = function.lock() {
                function.clone()
            } else {
                todo!()
            }
        } else {
            debug!("Invalid type: cannot call a non function type");
            todo!("manage error handling for abstract execution");
        };

        let scope_len = function.inner.inner.len() + function.args.len() + captures.len();
		let scope_id = self.new_id();
		let mut decls: Vec<String> = function.inner
            .decls
            .iter()
            .map(|id| format!("{}::{}", id, scope_id))
            .collect();
		decls.append(&mut function.args.iter().map(|id| format!("{}::{}", id, scope_id)).collect());
		decls.append(&mut captures.iter().map(|(id, _)| format!("{}::{}", id, scope_id)).collect());
        let scope = if let Some(write) = write {
            let value = BoxVariable::default();
            let job = Job {
                inner: EJob::Write((write, value.clone(), decls)),
                scope: job.scope,
                next: job.next,
            };
            Arc::new(Scope {
                id: scope_id,
                len: AtomicU64::new(scope_len as u64),
                value,
                job: Some(job),
            })
        } else {
            let value = if latest {
                job.scope.value.clone()
            } else {
                Default::default()
            };

            let compound = Job {
                inner: EJob::Empty(decls),
                next: job.next,
                scope: job.scope,
            };

            Arc::new(Scope {
                id: scope_id,
                len: AtomicU64::new(scope_len as u64),
                value,
                job: Some(compound),
            })
        };

        for (index, arg) in function.args.into_iter().enumerate() {
            // note: I think this check can be avoid if not in abstract execution.
            let param = if index < call.params.len() {
                call.params[index].clone()
            } else {
                debug!("missing argument");
                todo!("manage missing argument, with an error or Empty type");
            };
            // Create a new assignation tree.
            let inner = EExpression::Assignation(Assignation {
                block_on: false,
                var: arg,
                to_assign: param,
            });
            self.schedule(Job {
                inner: EJob::Expression(Expression {
                    latest: false,
                    inner,
                }),
                next: None,
                scope: scope.clone(),
            });
        }

        // Define captured variables in the execution flow
        for (tag, val) in captures {
            let ptr = Arc::new(AtomicPtr::new(Box::into_raw(Box::new(val))));
            let job = Job {
                inner: EJob::Write((format!("{}::{}", tag, scope.id), ptr, vec![])),
                scope: scope.clone(),
                next: None,
            };
            self.schedule(job);
        }

        debug!("schedule expressions of function {}", &call.name);
        self.expressions(&function.inner.inner, scope);
    }

    fn exec(&self, job: Job) {
        match &job.inner {
            EJob::Expression(expr) => {
                let latest = expr.latest;
                match &expr.inner {
                    EExpression::Statement(stat) => match &stat.inner {
                        EStatement::Compound(input) => {
                            debug!("create a new scope from a scope");
                            let value = if latest {
                                job.scope.value.clone()
                            } else {
                                Default::default()
                            };
							let new_scope_id = self.new_id();
                            let decls = input
                                .decls
                                .iter()
                                .map(|id| format!("{}::{}", id, new_scope_id))
                                .collect();
                            let compound = Job {
                                inner: EJob::Empty(decls),
                                next: job.next,
                                scope: job.scope,
                            };
                            let scope = Arc::new(Scope {
                                id: new_scope_id,
                                len: AtomicU64::new(input.inner.len() as u64),
                                value,
                                job: Some(compound),
                            });
                            self.expressions(&input.inner, scope);
                        }
                        EStatement::Str(val) => {
                            if latest {
                                let boxed =
                                    Box::new(Arc::new(Variable::String(Mutex::new(val.clone()))));
                                job.scope
                                    .value
                                    .store(Box::into_raw(boxed), Ordering::SeqCst);
                            } else {
                                debug!("dead string expression spoted");
                            }
                            self.complete_job(job);
                        }
                        EStatement::Call(call) => {
                            self.call_statement(call, job.clone(), latest, None)
                        }
                        EStatement::Copy(_v) => todo!(),
                        EStatement::Ref(v) => {
                            if latest {
                                if let Some(val) = self.find(&v, &job) {
                                    let boxed = Box::new(val);
                                    job.scope
                                        .value
                                        .store(Box::into_raw(boxed), Ordering::SeqCst);
                                } else {
                                    self.schedule_later(job);
                                    return;
                                }
                            }
                            self.complete_job(job);
                        }
                        EStatement::Function(v) => {
                            if latest {
                                let mut captures = vec![];
                                for c in &v.captures {
                                    if let Some(var) = self.find(c, &job) {
                                        captures.push((c.clone(), var));
                                    } else {
                                        self.schedule_later(job);
                                        return;
                                    }
                                }
                                let boxed = Box::new(Arc::new(Variable::Function(Mutex::new((
                                    v.clone(),
                                    captures,
                                )))));
                                job.scope
                                    .value
                                    .store(Box::into_raw(boxed), Ordering::SeqCst);
                            } else {
                                debug!("dead string expression spoted");
                            }
                            self.complete_job(job);
                        }
                    },
                    EExpression::Assignation(assignation) => {
                        self.assignation(&assignation, job.clone());
                    }
                    EExpression::Declaration(assignation) => {
                        self.assignation(&assignation, job.clone());
                    }
                }
            }
            EJob::Write((tag, value, decls)) => {
                if let Ok(vars) = &mut self.variables.lock() {
                    let value = *unsafe { Box::from_raw(value.load(Ordering::SeqCst)) };
                    debug!("EJob::Write {:?} into {}", value, tag);
                    vars.insert(tag.clone(), value);
                }
                self.schedule(Job {
					inner: EJob::Delete(decls.clone()),
					next: None,
					scope: job.scope.clone(),
				});
                self.complete_job(job);
            }
            EJob::Delete(decls) => {
                debug!("delete {:?} requested", decls);
                if let Ok(vars) = &mut self.variables.lock() {
					for decl in decls {
						if vars.remove(decl).is_none() {
							debug!("Error: {decl} does not exist in memory");
						}
					}
				}
			}
            EJob::Empty(decls) => {
				self.schedule(Job {
					inner: EJob::Delete(decls.clone()),
					next: None,
					scope: job.scope.clone(),
				});
                self.complete_job(job);
			}
            EJob::Expressions(_) => panic!("batch execution not covered"),
        }
    }
}
