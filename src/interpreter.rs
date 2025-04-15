use crate::exec_tree::*;
use crate::memory::{self, *};

use std::collections::HashMap;
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

/// Tracking function calls in abstract context.
#[derive(Default, PartialEq, Eq, Hash)]
struct FunctionCall {
    /// Function ID
    id: String,
    /// Variable inputs
    inputs: Vec<(String, Arc<Variable>)>,
    /// Variable output
    output: Arc<Variable>,
}

#[derive(Clone)]
enum EJob {
    Expression(Expression),
    /// Case 1: One job is blocking and next job is a list of
    ///         expressions
    /// Case 2: Abstract interpretation need to wait for input
    ///         before running the function compound statement.
    Expressions(Vec<Expression>),
    /// Write value from box into memory (end scope)
    Write(
        (
            String,
            BoxVariable,
            Vec<String>, /* variables declared in the scope */
        ),
    ),
    /// Free scope
    Delete(Vec<String>),
    /// End of scope
    Empty((BoxVariable, Vec<String>)),
}

#[derive(Clone)]
pub struct Job {
    /// Kind of job it contains.
    inner: EJob,
    /// Jobs to be executed after completing this one.
    next: Option<EJob>,
    /// Scope of the current job.
    pub scope: Arc<Scope>,
    /// Related function call of this Job. A parameter
    /// assignation, an external reference assignation (inputs)
    /// or the main compound of a call (output) have a function
    /// call.
    fc: Option<Arc<Mutex<FunctionCall>>>,
}

pub struct Scope {
    pub id: u64,
    len: AtomicU64,
    value: BoxVariable,
    /// Parent job (can be Write or Empty), this is filled on function call
    /// and entering a compound statement.
    pub job: Option<Job>,
    memory: Arc<Memory>,
}

pub struct Interpreter {
    jobs: Arc<Mutex<Vec<Job>>>,
    counter: AtomicU64,
    is_abstract: bool,
    /// Dictionary of all observed functions during abstract execution.
    functions: Arc<Mutex<HashMap<String, Function>>>,
    /// Set of resolved function calls during abstract execution. (a resolved
    /// function call is observed for when a function has been called with
    /// some parameters and has produced a reponse, so we know his signature:
    /// inputs(X1, X2, ...) -> output(Y))
    resolved_function_calls: Arc<
        Mutex<HashMap<(String, Vec<(String, Arc<Variable>)> /* Xi */), Arc<Variable> /* Y */>>,
    >,
}

impl Interpreter {
    pub fn default() -> Self {
        Interpreter {
            jobs: Default::default(),
            counter: Default::default(),
            is_abstract: false,
            functions: Default::default(),
            resolved_function_calls: Default::default(),
        }
    }

    pub fn abstr() -> Self {
        Interpreter {
            jobs: Default::default(),
            counter: Default::default(),
            is_abstract: true,
            functions: Default::default(),
            resolved_function_calls: Default::default(),
        }
    }

    fn new_id(&self) -> u64 {
        self.counter.fetch_add(1, Ordering::SeqCst)
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
                memory: Default::default(),
                job: None,
            }),
        );

        loop {
            let job = if let Ok(jobs) = &mut self.jobs.lock() {
                if let Some(job) = jobs.pop() {
                    job.clone()
                } else {
                    break;
                }
            } else {
                continue;
            };
            self.exec(job);
        }

        if self.is_abstract {
            debug!("Start checking functions calls");
            for (id, f1) in self.functions.lock().unwrap().iter() {
                debug!("check function {}", id);
                for f2 in &*f1.same_as.lock().unwrap() {
                    debug!("check if {} == {}", id, f2.id);
                }
            }
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
                        fc: None,
                    }
                }
                EExpression::Assignation(input) => {
                    blocking = input.block_on;
                    Job {
                        inner: EJob::Expression(expr.clone()),
                        next: None,
                        scope: scope.clone(),
                        fc: None,
                    }
                }
                EExpression::Declaration(input) => {
                    blocking = input.block_on;
                    Job {
                        inner: EJob::Expression(expr.clone()),
                        next: None,
                        scope: scope.clone(),
                        fc: None,
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
                    fc: None,
                };

                let scope = Arc::new(Scope {
                    id: new_scope_id,
                    len: AtomicU64::new(input.inner.len() as u64),
                    memory: memory::push(job.scope.memory.clone()),
                    value,
                    job: Some(job),
                });
                self.expressions(&input.inner, scope);
            }
            EStatement::Str(val) => {
                let key = format!("{}::{}", assign.var, job.scope.id);
                if self.is_abstract {
                    let val = memory::abstract_string();
                    job.scope.memory.abstr_write(key, val);
                } else {
                    let val = memory::string(val);
                    job.scope.memory.write(key, val);
                }
                self.complete_job(job);
            }
            EStatement::Function(f) => {
                debug!("Declare a function");
                let mut captures = vec![];
                for c in &f.captures {
                    if let Some(var) = job.scope.memory.find(c, &job) {
                        captures.push((c.clone(), var));
                    } else {
                        self.schedule_later(job);
                        return;
                    }
                }
                let key = format!("{}::{}", assign.var, job.scope.id);
                debug!("Write {:?} into {}", f, key);

                if self.is_abstract {
                    if let Ok(functions) = &mut self.functions.lock() {
                        if !functions.contains_key(&f.id) {
                            functions.insert(f.id.clone(), f.clone());
                        }
                    }
                    job.scope
                        .memory
                        .abstr_write(key, memory::function(f.clone(), captures));
                } else {
                    job.scope
                        .memory
                        .write(key, memory::function(f.clone(), captures));
                }
                self.complete_job(job);
            }
            EStatement::Call(c) => {
                debug!("Execute Call statement in assignation");
                let id = format!("{}::{}", assign.var, job.scope.id);
                self.call_statement(c, job, false, Some(id))
            }
            EStatement::Copy(_c) => todo!(),
            EStatement::Ref(c) => {
                debug!("try to assign {} from {c}", assign.var);
                let r = job.scope.memory.find(c, &job);
                if r.is_none() {
                    self.schedule_later(job);
                    return;
                }
                let val = r.unwrap();
                let key = format!("{}::{}", assign.var, job.scope.id);
                debug!("assign {} from {c}, {:?}", assign.var, val);
                if self.is_abstract {
                    job.scope.memory.abstr_write(key, val);
                } else {
                    job.scope.memory.write(key, val);
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
        let function = if let Some(function) = job.scope.memory.find(&call.name, &job) {
            function
        } else {
            debug!("function not found");
            self.schedule_later(job);
            return;
        };

        // From memory, get the function definition and the captured
        // variables (external references)
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

        // Compute the scope len which is the compound length, the functions arguments
        // to resolves and the captured variables to setup on the fly, all additionned.
        let scope_len = function.inner.inner.len() + function.args.len() + captures.len();
        let scope_id = self.new_id();

        // Keep track of all declared variables for logging and maybe for a better
        // management of memory later.
        let mut decls: Vec<String> = function
            .inner
            .decls
            .iter()
            .map(|id| format!("{}::{}", id, scope_id))
            .collect();
        decls.append(
            &mut function
                .args
                .iter()
                .map(|id| format!("{}::{}", id, scope_id))
                .collect(),
        );
        decls.append(
            &mut captures
                .iter()
                .map(|(id, _)| format!("{}::{}", id, scope_id))
                .collect(),
        );

        // If abstract, generate a FunctionCall that keep track of all
        // scope input and output. Setup the inputs here.
        let function_call = if self.is_abstract {
            let mut inputs = vec![];
            inputs.append(
                &mut function
                    .args
                    .iter()
                    .map(|id| (id.clone(), memory::abstract_uninit()))
                    .collect(),
            );
            inputs.append(
                &mut captures
                    .iter()
                    .map(|(id, _)| (id.clone(), memory::abstract_uninit()))
                    .collect(),
            );
            Some(Arc::new(Mutex::new(FunctionCall {
                id: call.name.clone(),
                inputs,
                output: memory::abstract_uninit(),
            })))
        } else {
            None
        };

        // Compute new scope, if the scope result is written in a variable,
        // schedule a Write job as parent with a new box variable. Otherwise,
        // make return value percolate to the upper scope copying the upper
        // scope `value` reference.
        let scope = if let Some(write) = write {
            let value = BoxVariable::default();
            let job = Job {
                inner: EJob::Write((write, value.clone(), decls)),
                scope: job.scope,
                next: job.next,
                fc: function_call.clone(),
            };
            Arc::new(Scope {
                id: scope_id,
                len: AtomicU64::new(scope_len as u64),
                value,
                memory: memory::push(job.scope.memory.clone()),
                job: Some(job),
            })
        } else {
            // Percolate only if the call is the latest expression
            // in the upper scope.
            let value = if latest {
                debug!("use top scope value");
                job.scope.value.clone()
            } else {
                debug!("create default scope value");
                if self.is_abstract {
                    debug!("create default scope value (abstract setup)");
                    let boxed = Box::new(memory::abstract_uninit());
                    Arc::new(AtomicPtr::new(Box::into_raw(boxed)))
                } else {
                    Default::default()
                }
            };

            let memory = memory::push(job.scope.memory.clone());

            let compound = Job {
                inner: EJob::Empty((value.clone(), decls)),
                next: job.next,
                scope: job.scope,
                fc: function_call.clone(),
            };

            Arc::new(Scope {
                id: scope_id,
                len: AtomicU64::new(scope_len as u64),
                value,
                memory,
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
                fc: None,
            });
        }

        // Define captured variables in the execution flow
        for (tag, val) in captures {
            let ptr = Arc::new(AtomicPtr::new(Box::into_raw(Box::new(val))));
            let job = Job {
                inner: EJob::Write((format!("{}::{}", tag, scope.id), ptr, vec![])),
                scope: scope.clone(),
                next: None,
                fc: None,
            };
            debug!("schedule writing {}::{}", tag, scope.id);
            self.schedule(job);
        }

        // If the interpreter is abstract, we want first to resolve the input
        // types. Then, if the function call is considered as already resolved
        // by the interpreter (registred in this set on function calls) we'll
        // be able to return the output already processed. If it's considered
        // as not resolved, we process the inner expression.
        if self.is_abstract {
            self.schedule_later(Job {
                inner: EJob::Expressions(function.inner.inner),
                scope,
                next: None,
                fc: function_call,
            });
        } else {
            debug!("schedule expressions of function {}", &call.name);
            self.expressions(&function.inner.inner, scope);
        }
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
                                if self.is_abstract {
                                    let boxed = Box::new(memory::abstract_uninit());
                                    Arc::new(AtomicPtr::new(Box::into_raw(boxed)))
                                } else {
                                    Default::default()
                                }
                            };
                            let new_scope_id = self.new_id();
                            let decls = input
                                .decls
                                .iter()
                                .map(|id| format!("{}::{}", id, new_scope_id))
                                .collect();

                            let memory = memory::push(job.scope.memory.clone());

                            let compound = Job {
                                inner: EJob::Empty((value.clone(), decls)),
                                next: job.next,
                                scope: job.scope,
                                fc: None,
                            };

                            let scope = Arc::new(Scope {
                                id: new_scope_id,
                                len: AtomicU64::new(input.inner.len() as u64),
                                value,
                                memory,
                                job: Some(compound),
                            });
                            self.expressions(&input.inner, scope);
                        }
                        EStatement::Str(val) => {
                            if latest {
                                let boxed = if self.is_abstract {
                                    Box::new(memory::abstract_string())
                                } else {
                                    Box::new(memory::string(val))
                                };
                                debug!("set scope value (str expr)");
                                job.scope
                                    .value
                                    .store(Box::into_raw(boxed), Ordering::SeqCst);
                            } else {
                                debug!("dead string expression spoted");
                            }
                            self.complete_job(job);
                        }
                        EStatement::Call(call) => {
                            debug!("Execute call statement");
                            self.call_statement(call, job.clone(), latest, None)
                        }
                        EStatement::Copy(_v) => todo!(),
                        EStatement::Ref(v) => {
                            if latest {
                                if let Some(val) = job.scope.memory.find(&v, &job) {
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
                                    if let Some(var) = job.scope.memory.find(c, &job) {
                                        captures.push((c.clone(), var));
                                    } else {
                                        self.schedule_later(job);
                                        return;
                                    }
                                }
                                let boxed = Box::new(memory::function(v.clone(), captures));
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
                let value = *unsafe { Box::from_raw(value.load(Ordering::SeqCst)) };
                debug!("EJob::Write {:?} into {}", value, tag);
                if self.is_abstract {
                    if let Some(fc) = &job.fc {
                        let fc = &mut fc.lock().unwrap();
                        fc.output = value.clone();
                        match &*value {
                            Variable::Abstract(_) => {}
                            Variable::Function(_) => {}
                            _ => panic!("non abstract type"),
                        }
                        debug!("push new resovled function (Write)");
                        self.resolved_function_calls
                            .lock()
                            .unwrap()
                            .insert((fc.id.clone(), fc.inputs.clone()), fc.output.clone());
                    }
                    job.scope.memory.abstr_write(tag.clone(), value);
                } else {
                    job.scope.memory.write(tag.clone(), value);
                }
                self.schedule(Job {
                    inner: EJob::Delete(decls.clone()),
                    next: None,
                    scope: job.scope.clone(),
                    fc: None,
                });
                self.complete_job(job);
            }
            EJob::Delete(decls) => {
                debug!("delete {:?} requested", decls);
            }
            EJob::Empty((value, decls)) => {
                if self.is_abstract {
                    if let Some(fc) = &job.fc {
                        debug!("get value (fc + abstract + empty)");
                        let value = *unsafe { Box::from_raw(value.load(Ordering::SeqCst)) };
                        debug!("got value (fc + abstract + empty)");
                        let fc = &mut fc.lock().unwrap();
                        fc.output = value.clone();
                        match &*value {
                            Variable::Abstract(_) => {}
                            Variable::Function(_) => {}
                            _ => panic!("non abstract type"),
                        }
                        debug!("push new resovled function");
                        self.resolved_function_calls
                            .lock()
                            .unwrap()
                            .insert((fc.id.clone(), fc.inputs.clone()), fc.output.clone());
                    }
                }
                self.schedule(Job {
                    inner: EJob::Delete(decls.clone()),
                    next: None,
                    scope: job.scope.clone(),
                    fc: None,
                });
                self.complete_job(job);
            }
            EJob::Expressions(exprs) => {
                if self.is_abstract {
                    let fc = job
                        .fc
                        .clone()
                        .expect("abstract interpretation must have function call tracking");
                    if {
                        let fc = &mut fc.lock().unwrap();
                        let id = fc.id.clone();
                        for (name, ty) in fc.inputs.iter_mut() {
                            if let Variable::Abstract(AbstractVariable::Uninit) = **ty {
                                debug!("abstract call of {} waiting for {}", id, name);
                                if let Some(v) = job.scope.memory.find(name, &job) {
                                    debug!("{} found", name);
                                    *ty = v.clone();
                                } else {
                                    debug!("{} still undefined", name);
                                }
                            }
                        }
                        debug!("{:?}", fc.inputs);
                        fc.inputs.iter().any(|(_, ty)| {
                            if let Variable::Abstract(AbstractVariable::Uninit) = **ty {
                                true
                            } else {
                                false
                            }
                        })
                    } {
                        // There is still inputs that have to be initialized.
                        // Abstract execution require function call input to be
                        // ready before being processed.
                        debug!("reschedule call");
                        self.schedule_later(job);
                        return;
                    }

                    let fc = &mut fc.lock().unwrap();
                    // All input are ready, check if we already resolved the function
                    // call.
                    if let Some(output) = self
                        .resolved_function_calls
                        .lock()
                        .unwrap()
                        .get(&(fc.id.clone(), fc.inputs.clone()))
                    {
                        debug!("skip function call because already checked");
                        let boxed = Box::new(output.clone());
                        job.scope
                            .value
                            .store(Box::into_raw(boxed), Ordering::SeqCst);
                        self.complete_job(job);
                    } else {
                        debug!("execute function expression: {:?}", exprs);
                        self.expressions(exprs, job.scope);
                    }
                } else {
                    debug!("unmanaged job detected")
                }
            }
        }
    }
}
