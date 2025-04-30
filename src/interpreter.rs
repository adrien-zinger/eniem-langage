use crate::builtins::*;
use crate::exec_tree::*;
use crate::libc::*;
use crate::memory::{self, *};

use std::collections::{HashMap, VecDeque};
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
    /// Builtin function as printf, itoa, i32_add...
    Builtin(Call),
    /// Case 1: One job is blocking and next job is a list of
    ///         expressions. Not managed by the exec function.
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

type FunctionCalls = HashMap<Vec<(String, Arc<Variable>)>, Arc<Variable>>;

/// HashMap(key: Function ID, value: HashMap(key: Inputs, value: Output))
/// with Inputs: [(variable ID wo scope ID, Variable)]
/// and Output: Variable
#[derive(Default)]
struct FunctionCallsDictionary(HashMap<String, FunctionCalls>);

impl FunctionCallsDictionary {
    fn insert(
        &mut self,
        (id, inputs): (String, Vec<(String, Arc<Variable>)>),
        output: Arc<Variable>,
    ) {
        self.0.entry(id).or_default().insert(inputs, output);
    }

    fn get(&self, id: &str, inputs: &Vec<(String, Arc<Variable>)>) -> Option<Arc<Variable>> {
        self.0.get(id)?.get(inputs).cloned()
    }

    fn get_function_calls(&self, id: &str) -> Option<&FunctionCalls> {
        self.0.get(id)
    }
}

#[derive(Clone)]
pub struct Interpreter {
    jobs: Arc<Mutex<VecDeque<Job>>>,
    /// Counter used to create unique ID.
    counter: Arc<AtomicU64>,
    /// True if the execution is abstract.
    is_abstract: bool,
    /// Dictionary of all observed functions during abstract execution.
    /// Key: Function ID, Value: Function copy.
    functions: Arc<Mutex<HashMap<String, Function>>>,
    /// Set of resolved function calls during abstract execution. (a resolved
    /// function call is observed for when a function has been called with
    /// some parameters and has produced a reponse, so we know his signature:
    /// inputs(X1, X2, ...) -> output(Y))
    resolved_function_calls: Arc<Mutex<FunctionCallsDictionary>>,
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

        if self.is_abstract {
            loop {
                let job = if let Ok(jobs) = &mut self.jobs.lock() {
                    if let Some(job) = jobs.pop_back() {
                        job.clone()
                    } else {
                        break;
                    }
                } else {
                    continue;
                };
                self.exec(job);
            }

            self.check_functions_types();
        } else {
            for _ in 0..=2 {
                let i1 = self.clone();
                std::thread::spawn(move || loop {
                    let job = if let Ok(jobs) = &mut i1.jobs.lock() {
                        if let Some(job) = jobs.pop_back() {
                            job.clone()
                        } else {
                            break;
                        }
                    } else {
                        continue;
                    };
                    i1.exec(job);
                });
            }
            loop {
                let job = if let Ok(jobs) = &mut self.jobs.lock() {
                    if let Some(job) = jobs.pop_back() {
                        job.clone()
                    } else {
                        break;
                    }
                } else {
                    continue;
                };
                self.exec(job);
            }
        }
    }

    fn schedule(&self, job: Job) {
        if let Ok(jobs) = &mut self.jobs.lock() {
            jobs.push_front(job);
        }
    }

    /// Create jobs for a list of expressions. Doesn't execute things
    /// as the exec function, assignation or call_statement. Just scheduling
    /// jobs.
    fn expressions(&self, exprs: &[Expression], scope: Arc<Scope>) {
        debug!("get {} expressions", exprs.len());
        for (index, expr) in exprs.iter().enumerate() {
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
                EExpression::Using(n) => todo!(),
            };

            // todo test in another pass if 'await' is on the last expression
            // and remove the anotation. (with a compilation warning to the user)
            if blocking && index < exprs.len() - 1 {
                debug!("postpawn blocks\n{:#?}", exprs[index + 1..].to_vec());
                job.next = Some(EJob::Expressions(exprs[index + 1..].to_vec()));
                self.schedule(job);
                break;
            } else {
                self.schedule(job);
            }
        }
    }

    fn assignation(&self, assign: &Assignation, job: Job) {
        debug!("enter assignation, job scope id {}", job.scope.id);
        match &assign.to_assign.inner {
            EStatement::Compound(input) => {
                debug!("assignation create a scope");
                let value = BoxVariable::default();
                debug!("scope decls: {:?}", input.decls);
                debug!("scope refs: {:?}", assign.to_assign.refs);
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
                    memory: job.scope.memory.new(&assign.to_assign.refs, job.scope.id),
                    value,
                    job: Some(job),
                });
                self.expressions(&input.inner, scope);
            }
            EStatement::Str(val) => {
                let key = format!("{}::{}", assign.var, job.scope.id);
                debug!("assign a string to {key}");
                if self.is_abstract {
                    let val = memory::abstract_string();
                    job.scope.memory.abstr_write(key, val);
                } else {
                    let val = memory::string(val);
                    job.scope.memory.write(key, val);
                }
                self.complete_job(job);
            }
            EStatement::Num(val) => {
                let key = format!("{}::{}", assign.var, job.scope.id);
                debug!("assign a number to {key}");
                if self.is_abstract {
                    let val = memory::abstract_number();
                    job.scope.memory.abstr_write(key, val);
                } else {
                    let val = memory::number(*val);
                    job.scope.memory.write(key, val);
                }
                self.complete_job(job);
            }
            EStatement::Function(f) => {
                debug!("Declare a function");
                debug!("function refs: {:?}", assign.to_assign.refs);
                let mut captures = vec![];
                for c in &f.captures {
                    if let Some(var) = job.scope.memory.find(c, &job) {
                        captures.push((c.clone(), var));
                    } else {
                        self.schedule(job);
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
            EStatement::StdCall(call) => {
                let id = format!("{}::{}", assign.var, job.scope.id);
                self.std_call_statement(call, job, false, Some(id))
            }
            EStatement::Copy(_c) => todo!(),
            EStatement::Ref(c) => {
                debug!("try to assign {} from {c}", assign.var);
                let r = job.scope.memory.find(c, &job);
                if r.is_none() {
                    debug!("retry to assign {} later", assign.var);
                    self.schedule(job);
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
        if let Some(EJob::Expressions(exprs)) = job.next {
            self.expressions(&exprs, job.scope);
        }
    }

    /// Execute a standard call statement like a libc call (printf, itoa...) or a
    /// builtin (i32_add, u32_mult...).
    ///  call: reference to the call statement.
    ///  job: copy of the full job containing the statement.
    ///  latest: is it the latest expression of the current scope.
    ///  write: Some if the result has to be assigned to something, otherwise None.
    fn std_call_statement(&self, call: &Call, job: Job, latest: bool, write: Option<String>) {
        // Identifier of 2 scopes, the one which contains the builtin call and the
        // one which initialise the arguments.
        let scope_id = self.new_id();

        // Compute a new scope, if the scope result is written in a variable,
        // schedule a Write job as parent with a new box variable. Otherwise,
        // make return value percolate to the upper scope copying the upper
        // scope `value` reference.
        let scope = if let Some(write) = write {
            let value = BoxVariable::default();
            let job = Job {
                inner: EJob::Write((write, value.clone(), vec![])),
                scope: job.scope,
                next: job.next,
                fc: None,
            };
            Arc::new(Scope {
                id: scope_id,
                len: AtomicU64::new(1),
                value,
                memory: job.scope.memory.clone(),
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

            let memory = job.scope.memory.clone();

            let compound = Job {
                inner: EJob::Empty((value.clone(), vec![])),
                next: job.next,
                scope: job.scope,
                fc: None,
            };

            Arc::new(Scope {
                id: scope_id,
                len: AtomicU64::new(1),
                value,
                memory,
                job: Some(compound),
            })
        };

        // Create again a new scope containing parameters assignation. The parent
        // Job is a call to the builtin, triggered after all assignations.

        let memory = scope.memory.clone();

        let builtin_job = Job {
            inner: EJob::Builtin(call.clone()),
            next: None,
            scope,
            fc: None,
        };

        let param_scope = Arc::new(Scope {
            id: scope_id, // no need to add a new scope layer.
            len: AtomicU64::new(call.params.len() as u64),
            value: Default::default(),
            memory, // same memory as upper scope. (no declarations expecteds)
            job: Some(builtin_job),
        });

        for (index, param) in call.params.iter().cloned().enumerate() {
            // Create a new assignation tree.
            let inner = EExpression::Assignation(Assignation {
                block_on: false,
                // variable doesn't have name.
                // simply index + unique scope id.
                var: format!("{index}"),
                to_assign: param,
            });

            self.schedule(Job {
                inner: EJob::Expression(Expression {
                    latest: false,
                    inner,
                }),
                next: None,
                scope: param_scope.clone(),
                fc: None,
            });
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
            debug!("function not found {}", call.name);
            self.schedule(job);
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
            panic!("manage error handling for abstract execution {:#?}", call);
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
                id: function.id.clone(),
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
                memory: job.scope.memory.clone(),
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

            let memory = job.scope.memory.clone();

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
            self.schedule(Job {
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
                    EExpression::Statement(statement) => match &statement.inner {
                        EStatement::Compound(input) => {
                            debug!("create a new scope from a scope");
                            let value = if latest {
                                job.scope.value.clone()
                            } else if self.is_abstract {
                                let boxed = Box::new(memory::abstract_uninit());
                                Arc::new(AtomicPtr::new(Box::into_raw(boxed)))
                            } else {
                                Default::default()
                            };
                            let new_scope_id = self.new_id();
                            let decls = input
                                .decls
                                .iter()
                                .map(|id| format!("{}::{}", id, new_scope_id))
                                .collect();

                            debug!("scope refs: {:?}", statement.refs);
                            let memory = job.scope.memory.new(&statement.refs, job.scope.id);

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
                        EStatement::Num(val) => {
                            if latest {
                                let boxed = if self.is_abstract {
                                    Box::new(memory::abstract_number())
                                } else {
                                    Box::new(memory::number(*val))
                                };
                                debug!("set scope value (str expr)");
                                job.scope
                                    .value
                                    .store(Box::into_raw(boxed), Ordering::SeqCst);
                            } else {
                                debug!("dead number expression spoted");
                            }
                            self.complete_job(job);
                        }
                        EStatement::Call(call) => {
                            debug!("Execute call statement");
                            self.call_statement(call, job.clone(), latest, None)
                        }
                        EStatement::StdCall(call) => {
                            debug!("Execute std call statement");
                            self.std_call_statement(call, job.clone(), latest, None)
                        }
                        EStatement::Copy(_v) => todo!(),
                        EStatement::Ref(v) => {
                            if latest {
                                if let Some(val) = job.scope.memory.find(v, &job) {
                                    let boxed = Box::new(val);
                                    job.scope
                                        .value
                                        .store(Box::into_raw(boxed), Ordering::SeqCst);
                                } else {
                                    self.schedule(job);
                                    return;
                                }
                            }
                            self.complete_job(job);
                        }
                        EStatement::Function(v) => {
                            debug!("function refs: {:?}", statement.refs);
                            if latest {
                                let mut captures = vec![];
                                for c in &v.captures {
                                    if let Some(var) = job.scope.memory.find(c, &job) {
                                        captures.push((c.clone(), var));
                                    } else {
                                        self.schedule(job);
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
                        self.assignation(assignation, job.clone());
                    }
                    EExpression::Declaration(assignation) => {
                        self.assignation(assignation, job.clone());
                    }
                    EExpression::Using(n) => todo!(),
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
                        debug!("push new resolved function (Write) id: {}", fc.id);
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
            EJob::Delete(_decls) => {
                debug!("delete {:?} requested", _decls);
            }
            EJob::Builtin(call) => {
                let params: Vec<Arc<Variable>> = call
                    .params
                    .iter()
                    .enumerate()
                    .map(|(index, _)| {
                        job.scope
                            .memory
                            .get(&format!("{}::{}", index, job.scope.id))
                            .unwrap()
                    })
                    .collect();
                let res = if self.is_abstract {
                    // todo check parameters too.
                    match call.std {
                        StdFunction::Atoi => Box::new(abstract_atoi(params[0].clone()).unwrap()),
                        StdFunction::Itoa => todo!("itoa not implemented"),
                        StdFunction::I32add => Box::new(
                            abstract_i32_add(params[0].clone(), params[1].clone()).unwrap(),
                        ),
                        StdFunction::I32mult => Box::new(
                            abstract_i32_mult(params[0].clone(), params[1].clone()).unwrap(),
                        ),
                        StdFunction::Printf => Box::new(memory::abstract_number()),
                        _ => todo!(),
                    }
                } else {
                    debug!("call printf");
                    match call.std {
                        StdFunction::Atoi => Box::new(atoi(params[0].clone())),
                        StdFunction::Itoa => todo!("itoa not implemented"),
                        StdFunction::I32add => {
                            Box::new(i32_add(params[0].clone(), params[1].clone()))
                        }
                        StdFunction::I32mult => {
                            Box::new(i32_mult(params[0].clone(), params[1].clone()))
                        }
                        StdFunction::Printf => Box::new(memory::number(builtin_printf(&params))),
                        _ => todo!(),
                    }
                };
                debug!("set scope value (str expr)");
                job.scope.value.store(Box::into_raw(res), Ordering::SeqCst);
                self.complete_job(job);
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
                        debug!("push new resolved function (Empty), id: {}", fc.id);
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
                    debug!("start interpreting function compound (abstract)");
                    let fc = job
                        .fc
                        .clone()
                        .expect("abstract interpretation must have function call tracking");
                    let res = {
                        let fc = &mut fc.lock().unwrap();
                        debug!("fc locked");
                        #[cfg(feature = "debug_interpreter")]
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
                        debug!("fc inputs {:?}", fc.inputs);
                        fc.inputs.iter().any(|(_, ty)| {
                            matches!(**ty, Variable::Abstract(AbstractVariable::Uninit))
                        })
                    };
                    if res {
                        // There is still inputs that have to be initialized.
                        // Abstract execution require function call input to be
                        // ready before being processed.
                        debug!("reschedule call");
                        self.schedule(job);
                        return;
                    }

                    let fc = &mut fc.lock().unwrap();
                    // All input are ready, check if we already resolved the function
                    // call.
                    if let Some(output) = self
                        .resolved_function_calls
                        .lock()
                        .unwrap()
                        .get(&fc.id, &fc.inputs)
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
                }
            }
        }
    }

    /// Abstract interpreter use to check if function types are valid.
    /// Functions are assigned to mutable variable. It implies that
    /// if a function is modifyed to be replaced by another, each calls
    /// that have been registred for the old function must provide the same
    /// behavior in the new function, and reciprocally.
    fn check_functions_types(&self) {
        // track new functions checks, if it's still 0 after
        // a check run, break and return.
        let mut checks = 0;
        let base_scope = Arc::new(Scope {
            id: self.new_id(),
            len: Default::default(),
            value: Default::default(),
            memory: Default::default(),
            job: None,
        });
        loop {
            // Start check
            let functions = self.functions.lock().unwrap().clone();
            for (function_id, function) in functions.iter() {
                let function_calls_opt = self
                    .resolved_function_calls
                    .lock()
                    .unwrap()
                    .get_function_calls(function_id)
                    .cloned();
                if let Some(function_calls) = function_calls_opt {
                    // Others are functions observed to be assigned where the
                    // current `function` where in memory.
                    let others = function.same_as.lock().unwrap().clone();
                    for other in others.iter() {
                        debug!("check function {} and {} equality", function_id, other.id);
                        // Iter the old function calls.
                        for (inputs, output) in &function_calls {
                            debug!("inputs/output {:?} {:?}", inputs, output);
                            let other_output_opt = self
                                .resolved_function_calls
                                .lock()
                                .unwrap()
                                .get(&other.id, inputs)
                                .clone();
                            if let Some(other_output) = other_output_opt {
                                if other_output != *output {
                                    panic!(
                                        "failed to match return types for {} and {}",
                                        function_id, other.id
                                    );
                                }
                            } else {
                                debug!("{} doesn't have resolved yet for this instance", other.id);
                                // Simulate a new function call. Increment the tracking counter.
                                checks += 1;
                                let scope_id = self.new_id();
                                let memory = Arc::new(Memory::default());
                                // Write input in memory.
                                for (input_id, variable) in inputs {
                                    let key = format!("{}::{}", input_id, scope_id);
                                    memory.abstr_write(key, variable.clone());
                                }
                                let boxed = Box::new(memory::abstract_uninit());
                                let value = Arc::new(AtomicPtr::new(Box::into_raw(boxed)));
                                let fc = Arc::new(Mutex::new(FunctionCall {
                                    id: other.id.clone(),
                                    inputs: inputs.clone(),
                                    output: memory::abstract_uninit(),
                                }));
                                // Create Job that would write the return type eventually.
                                let res_job = Job {
                                    inner: EJob::Empty((value.clone(), vec![])),
                                    next: None,
                                    scope: base_scope.clone(),
                                    fc: Some(fc.clone()),
                                };
                                let scope = Arc::new(Scope {
                                    id: scope_id,
                                    job: Some(res_job),
                                    len: AtomicU64::new(1),
                                    value,
                                    memory,
                                });
                                // Create the Job containing the function's expressions.
                                let job = Job {
                                    inner: EJob::Expressions(other.inner.inner.clone()),
                                    fc: Some(fc.clone()),
                                    next: None,
                                    scope,
                                };
                                debug!("execute new expressions job");
                                self.exec(job);
                                debug!("executed");
                                // Execute jobs until the FIFO is empty.
                                loop {
                                    let job = if let Ok(jobs) = &mut self.jobs.lock() {
                                        if let Some(job) = jobs.pop_back() {
                                            job.clone()
                                        } else {
                                            break;
                                        }
                                    } else {
                                        continue;
                                    };
                                    self.exec(job);
                                }

                                debug!("access to fc");
                                let other_output = &fc.lock().unwrap().output;
                                if *other_output != *output {
                                    panic!(
                                        "invalid function assignation detected {:?} vs {:?}",
                                        other_output, output
                                    );
                                }
                                // end Running simulation
                            }
                        } // Iter through same functions
                    } // Iter through calls
                }
            } // Iter through functions
            if checks == 0 {
                /* nothing more has been checked, end of the function checking */
                debug!("function type checking end");
                break;
            } else {
                /* some new functions have been registred. Relaunch a new check */
                checks = 0;
            }
        } // Check loop
    }
}
