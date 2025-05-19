//! Entry point of a job execution.
//!
//! The interpreter have a job's queue, independently
//! any thread can pop a job from it and call the
//! interpreter with it by using the `pop` function
//! of the interpreter.

use std::sync::{
    atomic::{AtomicPtr, AtomicU64, Ordering},
    Arc,
};

use crate::interpreter::{
    job::{EJob, Job},
    Scope,
};
use crate::{
    builtins::*,
    libc::*,
    memory::{self, AbstractVariable, Variable},
    Interpreter,
};

use crate::exec_tree::*;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

impl Interpreter {
    /// Pop and execute a Job from the `jobs` queue.
    /// See `Interpreter::exec` in this file.
    pub fn pop(&self) -> bool {
        let job = if let Ok(jobs) = &mut self.jobs.lock() {
            if let Some(job) = jobs.pop_back() {
                job
            } else {
                return false;
            }
        } else {
            return true;
        };
        self.exec(job);
        return true;
    }

    fn exec_module(&self, compound: &Compound) -> bool {
        if compound.module.is_some()
            && !compound.initialized.load(Ordering::SeqCst)
            && compound
                .initialized
                .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
                .is_err()
        {
            todo!("return with no execution")
        }
        return false;
    }

    fn new_compound_scope(
        &self,
        scope: Arc<Scope>,
        next: Option<EJob>,
        statement: &Statement,
        compound: &Compound,
        latest: bool,
    ) -> Arc<Scope> {
        let value = if latest {
            scope.value.clone()
        } else if self.is_abstract {
            let boxed = Box::new(memory::abstract_uninit());
            Arc::new(AtomicPtr::new(Box::into_raw(boxed)))
        } else {
            Default::default()
        };

        let new_scope_id = self.new_id();
        let decls = compound
            .decls
            .iter()
            .map(|id| format!("{}::{}", id, new_scope_id))
            .collect();

        debug!("scope refs: {:?}", statement.refs);
        let memory = scope.memory.new(&statement.refs, new_scope_id, scope.id);

        let parent_job = Job {
            inner: EJob::Empty((value.clone(), decls)),
            next,
            scope,
            fc: None,
        };

        Arc::new(Scope {
            id: new_scope_id,
            len: AtomicU64::new(compound.inner.len() as u64),
            value,
            memory,
            job: Some(parent_job),
        })
    }

    /// Execute a *Statement Expression* with a compound form.
    ///
    /// When `exec_compound` is called:
    ///
    /// ```
    /// let a = { ... } /* not a compound, this is assignation */
    /// { ... }         /* this is a compound */
    /// mod { ... }     /* modules are specific compounds, exec_compound is called
    /// ```
    ///
    /// 1. If its a module, return, see `Interpreter::exec_module`.
    /// 2. Otherwise creates and schedule jobs from compound's expressions.
    fn exec_compound(
        &self,
        scope: Arc<Scope>,
        next: Option<EJob>,
        statement: &Statement,
        compound: &Compound,
        latest: bool,
    ) {
        if self.exec_module(compound) {
            return;
        }
        self.expressions(
            &compound.inner,
            self.new_compound_scope(scope, next, statement, compound, latest),
        );
    }

    /// Same as `Interpreter::exec_num` but with a String.
    fn exec_str(&self, val: String, job: Job, latest: bool) {
        if latest {
            let boxed = if self.is_abstract {
                Box::new(memory::abstract_string())
            } else {
                Box::new(memory::string(&val))
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

    /// Execute a number expression statement. This statement
    /// isn't a right part at least that it's contained in a
    /// compound.
    ///
    /// # Inputs
    /// - `val`: The num value stored as an i32.
    /// - `job`: The current job containing the current scope, the value, the expression, etc.
    /// - `latest`: Is that expression the latest of the current compound. If it is, the scope
    ///             value will be set to the number.
    ///
    /// # Abstract
    /// The abstract interpreter will put an abstract number instead of the real `val`.
    fn exec_num(&self, val: i32, job: Job, latest: bool) {
        if latest {
            let boxed = if self.is_abstract {
                Box::new(memory::abstract_number())
            } else {
                Box::new(memory::number(val))
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

    /// Execute a Ref expression statement. Just an expression with a reference.
    fn exec_ref(&self, ref_id: &str, job: Job, latest: bool) {
        debug!("process job with single reference {:?}", v);
        if latest {
            if let Some(val) = job.scope.memory.find(ref_id, &job) {
                debug!("store value {:?}", val);
                let boxed = Box::new(val);
                job.scope
                    .value
                    .store(Box::into_raw(boxed), Ordering::SeqCst);
            } else {
                debug!("reschedule because reference not found");
                self.schedule(job);
                return;
            }
        }
        debug!("job complete");
        self.complete_job(job);
    }

    /// Execute an expression statement function declaration. Not a call.
    /// Put in memory the captured variable if found.
    fn exec_function(&self, function: &Function, job: Job, latest: bool) {
        debug!("function refs: {:?}", statement.refs);
        if latest {
            let mut captures = vec![];
            for c in &function.captures {
                if let Some(var) = job.scope.memory.find(c, &job) {
                    captures.push((c.clone(), var));
                } else {
                    self.schedule(job);
                    return;
                }
            }
            let boxed = Box::new(memory::function(function.clone(), captures));
            job.scope
                .value
                .store(Box::into_raw(boxed), Ordering::SeqCst);
        } else {
            debug!("dead string expression spoted");
        }
        self.complete_job(job);
    }

    /// Entry point to execute a Job.
    ///
    /// # Visibility
    /// It's to the interpreter itself to choose to execute a Job. Because all jobs
    /// are linked together. In the other hand, any process can call `Interpreter::pop(&self)`
    /// because it just exec something really scheduled in the queue and the interpreter can
    /// trust that the queued job have been generated by itself.
    pub(crate) fn exec(&self, job: Job) {
        match &job.inner {
            EJob::Expression(expr) => {
                let latest = expr.latest;
                match &expr.inner {
                    EExpression::Statement(statement) => match &statement.inner {
                        EStatement::Compound(compound) => {
                            self.exec_compound(job.scope, job.next, statement, compound, latest)
                        }
                        EStatement::Str(val) => self.exec_str(val.clone(), job, latest),
                        EStatement::Num(val) => self.exec_num(*val, job, latest),
                        EStatement::Call(call) => {
                            debug!("Execute call statement");
                            self.call_statement(call, job.clone(), latest, None, false)
                        }
                        EStatement::StdCall(call) => {
                            debug!("Execute std call statement");
                            self.std_call_statement(call, job.clone(), latest, None, false)
                        }
                        EStatement::Copy(_v) => todo!(),
                        EStatement::Ref(ref_id) => self.exec_ref(&ref_id.to_owned(), job, latest),
                        EStatement::Function(v) => self.exec_function(v, job.clone(), latest),
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
            EJob::Write((tag, value, decls, modify)) => {
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
                } else if *modify {
                    job.scope.memory.write_copy(tag.clone(), value);
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
                let mut params = vec![];
                for (index, _) in call.params.iter().enumerate() {
                    let param_opt = job
                        .scope
                        .memory
                        .get(&format!("{}::{}", index, job.scope.id));
                    if let Some(param) = param_opt {
                        params.push(param);
                    } else {
                        self.schedule(job);
                        return;
                    }
                }

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

                    debug!("abstract interpreter, function has all variable ready");

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
}
