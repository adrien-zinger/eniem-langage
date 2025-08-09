//! Entry point of a job execution.
//!
//! The interpreter have a job's queue, independently
//! any thread can pop a job from it and call the
//! interpreter with it by using the `pop` function
//! of the interpreter.

use crate::interpreter::job::{EJob, Job};

use super::{
    builtins::*,
    exec_tree::*,
    libc::*,
    memory::{self, AbstractVariable, Variable},
    Interpreter,
};

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
        true
    }

    /// Subfunction in dispatch statement expression. See `exec_expression`.
    fn exec_statement(&self, job: Job, statement: &Statement, latest: bool) {
        match &statement.inner {
            EStatement::Compound(compound) => self.exec_compound(job, statement, compound, latest),
            EStatement::Str(val) => self.exec_str(val.clone(), job, latest),
            EStatement::Num(val) => self.exec_num(*val, job, latest),
            EStatement::Bool(val) => self.exec_bool(*val, job, latest),
            EStatement::Call(call) => self.call_statement(call, job, latest, None, false).unwrap(),
            EStatement::StdCall(call) => self.std_call_statement(call, job, latest, None, false),
            EStatement::Copy(_v) => todo!(),
            EStatement::Ref(var) => self.exec_ref(&var.to_owned(), None, job, latest),
            EStatement::RefAs((var, ty)) => self.exec_ref(&var.to_owned(), Some(ty), job, latest),
            EStatement::Function(var) => self.exec_function(var, job, latest),
            EStatement::Branch(_branch) => todo!(),
        }
    }

    /// Dispatch execution to dedicated function for each kind of Expression.
    fn exec_expression(&self, job: Job, expr: &Expression) {
        match &expr.inner {
            EExpression::Statement(statement) => self.exec_statement(job, statement, expr.latest),
            EExpression::Assignation(assignation) => self.assignation(assignation, job),
            EExpression::Declaration(assignation) => self.assignation(assignation, job),
            EExpression::Using(_using) => todo!(),
        }
    }

    /// Execute a builtin. A builtin is a function sufixed with a `!`.
    /// Libc and functions like i32_add are builtins.
    ///
    /// A builtin require all parameters to be initialized and accessible
    /// in the scope where the builtins is called. If a reference cannot
    /// be found, then we reschedule the job for later.
    ///
    /// See `Interpreter::schedule(job)`.
    ///
    /// The job is complete when the libc or builtin has returned with success.
    fn exec_builtin(&self, job: Job, call: &Call) {
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
                StdFunction::Atoi => abstract_atoi(params[0].clone()).unwrap(),
                StdFunction::Itoa => todo!("itoa not implemented"),
                StdFunction::I32add => {
                    abstract_i32_add(params[0].clone(), params[1].clone()).unwrap()
                }
                StdFunction::I32mult => {
                    abstract_i32_mult(params[0].clone(), params[1].clone()).unwrap()
                }
                StdFunction::I32notEqual => {
                    abstract_i32_not_equal(params[0].clone(), params[1].clone()).unwrap()
                }
                StdFunction::Printf => memory::abstract_number(),
                _ => todo!(),
            }
        } else {
            match call.std {
                StdFunction::Atoi => atoi(params[0].clone()),
                StdFunction::Itoa => todo!("itoa not implemented"),
                StdFunction::I32add => i32_add(params[0].clone(), params[1].clone()),
                StdFunction::I32mult => i32_mult(params[0].clone(), params[1].clone()),
                StdFunction::I32notEqual => i32_not_equal(params[0].clone(), params[1].clone()),
                StdFunction::Printf => memory::number(builtin_printf(&params)),
                _ => todo!(),
            }
        };
        debug!("set scope value (str expr)");
        job.scope.set_value(res);
        self.complete_job(job);
    }

    /// Definitely try to execute a list of expression. That feature is used
    /// by the abstract interpreter only. It is used to check if functions
    /// type are not modifyied over the time.
    fn exec_expressions(&self, job: Job, exprs: &[Expression]) {
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
                fc.inputs
                    .iter()
                    .any(|(_, ty)| matches!(**ty, Variable::Abstract(AbstractVariable::Uninit)))
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
                job.scope.set_value(output.clone());
                self.complete_job(job);
            } else {
                debug!("execute function expression: {:?}", exprs);
                self.expressions(exprs, job.scope);
            }
        }
    }

    /// Entry point to execute a Job.
    ///
    /// # Visibility
    /// It's to the interpreter itself to choose to execute a Job. Because all jobs
    /// are linked together. In the other hand, any process can call `Interpreter::pop(&self)`
    /// because it just exec something really scheduled in the queue and the interpreter can
    /// trust that the queued job have been generated by itself.
    pub(crate) fn exec(&self, job: Job) {
        // note: exec_expression implemented in this file.
        match &*job.inner {
            EJob::Expression(expr) => self.exec_expression(job.to_owned(), expr),
            EJob::Write(wr) => self.exec_write(job.to_owned(), wr),
            EJob::Delete(_decls) => {
                debug!("delete {:?} requested", _decls)
            }
            EJob::Builtin(call) => self.exec_builtin(job.to_owned(), call),
            EJob::Empty((value, decls)) => self.exec_empty(job.to_owned(), value, decls),
            EJob::Expressions(exprs) => self.exec_expressions(job.to_owned(), exprs),
        }
    }
}
