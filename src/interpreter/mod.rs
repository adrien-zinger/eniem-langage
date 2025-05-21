//! Interpreter module.
//!
//! Contains both abstract interpreter and normal interpreter
//! implementation.
use crate::exec_tree::*;
use crate::memory::{self, *};

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

mod assignation;
mod exec;
pub(crate) mod job;

use job::*;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

pub(crate) struct Scope {
    pub id: u64,
    len: AtomicU64,
    value: BoxVariable,
    /// Parent job (can be Write or Empty), this is filled on function call
    /// and entering a compound statement.
    pub job: Option<Job>,
    memory: Arc<Memory>,
}

impl Scope {
    pub fn set_value(&self, var: Arc<Variable>) {
        let boxed = Box::new(var);
        self.value.store(Box::into_raw(boxed), Ordering::SeqCst);
    }

    /// Get a new value for a new scope that is a "child" of this scope.
    ///
    /// Set latest to true if you want the new scope position is at the end
    /// of the current scope. i.e. `{ let a = 1; {a} }`, `{a}` is at the end.
    /// It implies that the first compound return value IS the inner compound
    /// (`{a}` in the example) return value.
    ///
    /// Set is_abstract to true if it's an abstract context.
    pub fn get_new_value(&self, is_abstract: bool, latest: bool) -> BoxVariable {
        if latest {
            self.value.clone()
        } else if is_abstract {
            let boxed = Box::new(memory::abstract_uninit());
            Arc::new(AtomicPtr::new(Box::into_raw(boxed)))
        } else {
            Default::default()
        }
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
                        inner: EJob::Expression(expr.clone()).into(),
                        next: None,
                        scope: scope.clone(),
                        fc: None,
                    }
                }
                EExpression::Assignation(input) => {
                    blocking = input.block_on;
                    Job {
                        inner: EJob::Expression(expr.clone()).into(),
                        next: None,
                        scope: scope.clone(),
                        fc: None,
                    }
                }
                EExpression::Declaration(input) => {
                    blocking = input.block_on;
                    Job {
                        inner: EJob::Expression(expr.clone()).into(),
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
                job.next = Some(EJob::Expressions(exprs[index + 1..].to_vec()).into());
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
        if let Some(ejob) = job.next {
            if let EJob::Expressions(exprs) = &*ejob {
            	self.expressions(exprs, job.scope);
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
                                    inner: EJob::Empty((value.clone(), vec![])).into(),
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
                                    inner: EJob::Expressions(other.inner.inner.clone()).into(),
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
