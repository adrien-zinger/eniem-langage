//! Calls Job interpretation.
//!
//! Includes StdCalls (builtins invocations) and user function
//! calls.

use super::{
    exec_tree::*,
    job::*,
    memory::{self, *},
    FunctionCall, Interpreter, Scope,
};

use std::sync::{
    atomic::{AtomicPtr, AtomicU64},
    Arc, Mutex,
};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

impl Interpreter {
    fn new_empty_scope(
        &self,
        job: Job,
        decls: Vec<String>,
        function_call: &Option<Arc<Mutex<FunctionCall>>>,
        scope_len: usize,
        scope_id: u64,
        latest: bool,
    ) -> Arc<Scope> {
        let value = job.scope.get_new_value(self.is_abstract, latest);
        let memory = job.scope.memory.clone();
        let compound = Job {
            inner: EJob::Empty((value.clone(), decls)).into(),
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
    }

    fn new_write_scope(
        &self,
        job: Job,
        decls: Vec<String>,
        function_call: &Option<Arc<Mutex<FunctionCall>>>,
        scope_len: usize,
        scope_id: u64,
        tag: String,
        modify: bool,
    ) -> Arc<Scope> {
        let value = BoxVariable::default();
        let job = Job {
            inner: EJob::Write(WriteJob {
                tag,
                var: value.clone(),
                decls,
                modify,
            })
            .into(),
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
    }

    /// Call Statement tooling.
    ///
    /// Get function definition tree `Function` and captured variables `Inputs` from
    /// memory. The previous pass that ensure that `call.name` exist and is accessible
    /// from that point of the execution. Nevertheless, if the function hasn't been
    /// already defined, this function reschedule the Job and return None.
    ///
    /// Return Ok if the memory contains the function and is initialized. Err otherwise.
    /// If an Error is raised, the Job has been rescheduled.
    fn get_function_and_captures(&self, job: &Job, call: &Call) -> Result<(Function, Inputs), ()> {
        // find function in memory
        let function = if let Some(function) = job.scope.memory.find(&call.name, job) {
            function
        } else {
            debug!("function not found {}", call.name);
            //self.schedule(job.to_owned());
            return Err(());
        };
        // From memory, get the function definition and the captured
        // variables (external references)
        if let Variable::Function(function) = &*function {
            Ok(function.lock().unwrap().clone())
        } else {
            debug!("Invalid type: cannot call a non function type");
            panic!("manage error handling for abstract execution {:#?}", call);
        }
    }

    /// Call Statement tooling, get decls ids.
    /// Generate unique call site Ids for variables declared in the function
    /// compound, the function arguments and the captured variables.
    ///
    /// See `call_statement`.
    fn get_function_decls_ids(
        &self,
        function: &Function,
        captures: &Inputs,
        scope_id: u64,
    ) -> Vec<String> {
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
        decls
    }

    /// Call Statement tooling. Generate a new FunctionCall.
    /// Return Some(FunctionCall) if we are in an abstract context, None otherwise.
    fn get_function_call(
        &self,
        function: &Function,
        captures: &Inputs,
    ) -> Option<Arc<Mutex<FunctionCall>>> {
        if self.is_abstract {
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
        }
    }

    fn set_parameters(&self, function: &Function, call: &Call, function_scope: Arc<Scope>) {
        for (index, arg) in function.args.iter().cloned().enumerate() {
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
                })
                .into(),
                next: None,
                scope: function_scope.clone(),
                fc: None,
            });
        }
    }

    /// Call Statement tooling.
    /// Set captured variables into the new function scope.
    ///
    /// `val` is an Arc<Variable> and is in memory. That said, the scope and
    /// the memory region that initialized and used to contain the variable
    /// might have been destroyed. The Function have keeped a reference to it
    /// at call site.
    ///
    /// Schedule a new Job for each captured variables.
    ///
    /// ```
    /// let create_counter = () {
    ///      /* create i */
    ///      let i = 0;
    ///      /* clone a reference to i */
    ///      () { i = i + 1; i }
    ///      /* one reference to i is destroyed */
    /// }
    ///
    /// let c1 = create_counter()
    /// let c2 = create_counter()
    /// c1() /* return 1 */
    /// c1() /* return 2 */
    /// c2() /* return 1 */
    /// ```
    fn set_captured(&self, captures: Inputs, function_scope: Arc<Scope>) {
        for (tag, val) in captures {
            let ptr = Arc::new(AtomicPtr::new(Box::into_raw(Box::new(val))));
            let job = Job {
                inner: EJob::Write(WriteJob {
                    tag: format!("{}::{}", tag, function_scope.id),
                    var: ptr,
                    decls: vec![],
                    modify: false,
                })
                .into(),
                scope: function_scope.clone(),
                next: None,
                fc: None,
            };
            debug!("schedule writing {}::{}", tag, function_scope.id);
            self.schedule(job);
        }
    }

    /// Execute a call statement.
    ///  call: reference to the call statement.
    ///  job: copy of the full job containing the statement.
    ///  latest: is it the latest expression of the current scope.
    ///  write: Some if the result has to be assigned to something, otherwise None.
    ///  modify: In case of a write, should we modify or not the inner value.
    ///  retry: if function not found, retry later.
    pub(crate) fn call_statement(
        &self,
        call: &Call,
        job: Job,
        latest: bool,
        write: Option<String>,
        modify: bool,
        retry: bool,
    ) -> Result<(), ()> {
        // Inspect memory and find user function + captured variables.
        let fac_res = self.get_function_and_captures(&job, call);
        if let Err(err) = fac_res {
            if retry {
                self.schedule(job.to_owned());
            }
            return Err(err);
        }
        let (function, captures) = fac_res.unwrap();

        // Compute the scope len which is the compound length, the functions arguments
        // to resolves and the captured variables to setup on the fly, all additionned.
        let scope_len = function.inner.inner.len() + function.args.len() + captures.len();
        let scope_id = self.new_id();
        // Keep track of all declared variables.
        let decls = self.get_function_decls_ids(&function, &captures, scope_id);
        // Generate a FunctionCall.
        let function_call = self.get_function_call(&function, &captures);
        // Compute new scope.
        let function_scope = if let Some(tag) = write {
            self.new_write_scope(job, decls, &function_call, scope_len, scope_id, tag, modify)
        } else {
            self.new_empty_scope(job, decls, &function_call, scope_len, scope_id, latest)
        };
        // Set function parameters into new function scope.
        self.set_parameters(&function, call, function_scope.clone());
        // Define captured variables in the execution flow.
        self.set_captured(captures, function_scope.clone());
        // If the interpreter is abstract, we want first to resolve the input
        // types. Then, if the function call is considered as already resolved
        // by the interpreter (registred in this set on function calls) we'll
        // be able to return the output already processed. If it's considered
        // as not resolved, we process the inner expression.
        if self.is_abstract {
            self.schedule(Job {
                inner: EJob::Expressions(function.inner.inner).into(),
                scope: function_scope,
                next: None,
                fc: function_call,
            });
        } else {
            debug!("schedule expressions of function {}", &call.name);
            self.expressions(&function.inner.inner, function_scope);
        }
        Ok(())
    }

    /// Execute a standard call statement like a libc call (printf, itoa...) or a
    /// builtin (i32_add, u32_mult...).
    ///  call: reference to the call statement.
    ///  job: copy of the full job containing the statement.
    ///  latest: is it the latest expression of the current scope.
    ///  write: Some if the result has to be assigned to something, otherwise None.
    ///  modify: In case of a write, should we modify the inner value or the pointer.
    // todo: after the refact pub(super) should be alright.
    pub(crate) fn std_call_statement(
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
                inner: EJob::Write(WriteJob {
                    tag: write,
                    var: value.clone(),
                    decls: vec![],
                    modify,
                })
                .into(),
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
                inner: EJob::Empty((value.clone(), vec![])).into(),
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
            inner: EJob::Builtin(call.clone()).into(),
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
                })
                .into(),
                next: None,
                scope: param_scope.clone(),
                fc: None,
            });
        }
    }
}
