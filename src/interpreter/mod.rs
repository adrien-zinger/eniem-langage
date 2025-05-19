///! Interpreter module.
///!
///! Contains both abstract interpreter and normal interpreter
///! implementation.
use crate::builtins::*;
use crate::exec_tree::*;
use crate::libc::*;
use crate::memory::{self, *};

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

mod assignation;
mod exec;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

pub(super) struct Scope {
    pub id: u64,
    len: AtomicU64,
    value: BoxVariable,
    /// Parent job (can be Write or Empty), this is filled on function call
    /// and entering a compound statement.
    pub job: Option<Job>,
    memory: Arc<Memory>,
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
            bool,        /* modify or not */
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

        while self.pop() {}
        if self.is_abstract {
            self.check_functions_types();
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
                    debug!("schedule statement");
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

    /// Decrement scope len because we complete a Job and call nexts jobs.
    fn complete_job(&self, job: Job) {
        if job.scope.len.fetch_sub(1, Ordering::SeqCst) == 1 {
            if let Some(job) = &job.scope.job {
                // schedule parent (Write or Empty)
                self.schedule(job.clone());
            } else {
                #[cfg(not(test))] // tolerance in unit tests
                unreachable!("impossible to reach a scope complete with no parent")
            }
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
    ///  modify: In case of a write, should we modify the inner value or the pointer.
    fn std_call_statement(
        &self,
        call: &Call,
        job: Job,
        latest: bool,
        write: Option<String>,
        modify: bool,
    ) {
        debug!("enter std call {:?}", call);
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
                inner: EJob::Write((write, value.clone(), vec![], modify)),
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
                modify: true,
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
    ///  modify: In case of a write, should we modify or not the inner value.
    fn call_statement(
        &self,
        call: &Call,
        job: Job,
        latest: bool,
        write: Option<String>,
        modify: bool,
    ) {
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
                inner: EJob::Write((write, value.clone(), decls, modify)),
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
                modify: true,
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
                inner: EJob::Write((format!("{}::{}", tag, scope.id), ptr, vec![], false)),
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
                                // Write input in memory. Issue: captured variables
                                // are badly named. In addition, we must take into
                                // account calls with differents inputs length.
                                for ((_input_id, variable), arg) in
                                    inputs.iter().zip(other.args.iter())
                                {
                                    let key = format!("{}::{}", arg, scope_id);
                                    debug!("write variable {key}, {:?}", variable);
                                    memory.abstr_write(key, variable.clone());
                                }
                                let boxed = Box::new(memory::abstract_uninit());
                                let value = Arc::new(AtomicPtr::new(Box::into_raw(boxed)));
                                // Create a new function call from `other`
                                debug!("new function call id: {}", other.id);
                                debug!("new function call inputs given: {:#?}", inputs);
                                debug!("function expected inputs: {:#?}", other.args);
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
