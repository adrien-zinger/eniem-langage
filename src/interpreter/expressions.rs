use super::{
    exec_tree::{Compound, Function, Statement},
    job::{EJob, Job},
    memory, Interpreter, Scope,
};

use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

impl Interpreter {
    /// Execute a eniem's module expression.
    pub(super) fn exec_module(&self, compound: &Compound) -> bool {
        if compound.module.is_some()
            && !compound.initialized.load(Ordering::SeqCst)
            && compound
                .initialized
                .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
                .is_err()
        {
            todo!("return with no execution")
        }
        false
    }

    /// Create a new Scope for a compound.
    /// Used only for a left value. Assigned compound are managed
    /// in `assignation` module.
    fn new_compound_scope(
        &self,
        job: Job,
        statement: &Statement,
        compound: &Compound,
        latest: bool,
    ) -> Arc<Scope> {
        let value = job.scope.get_new_value(self.is_abstract, latest);
        let new_scope_id = self.new_id();
        let decls = compound
            .decls
            .iter()
            .map(|id| format!("{}::{}", id, new_scope_id))
            .collect();
        debug!("scope refs: {:?}", statement.refs);
        let memory = job
            .scope
            .memory
            .new(&statement.refs, new_scope_id, job.scope.id);
        // Create a parent Job Empty, without function call.
        let parent_job = Job {
            inner: EJob::Empty((value.clone(), decls)).into(),
            next: job.next,
            scope: job.scope,
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
    pub(super) fn exec_compound(
        &self,
        job: Job,
        statement: &Statement,
        compound: &Compound,
        latest: bool,
    ) {
        if self.exec_module(compound) {
            return;
        }
        self.expressions(
            &compound.inner,
            self.new_compound_scope(job, statement, compound, latest),
        );
    }

    /// Same as `Interpreter::exec_num` but with a String.
    pub(super) fn exec_str(&self, val: String, job: Job, latest: bool) {
        if latest {
            debug!("set scope value (str expr)");
            job.scope.set_value(if self.is_abstract {
                memory::abstract_string()
            } else {
                memory::string(&val)
            });
        } else {
            debug!("dead string expression spoted");
        }
        self.complete_job(job);
    }

    /// Same as `Interpreter::exec_num` but with a Boolean.
    pub(super) fn exec_bool(&self, val: bool, job: Job, latest: bool) {
        if latest {
            debug!("set scope value (str expr)");
            job.scope.set_value(if self.is_abstract {
                memory::abstract_boolean()
            } else {
                memory::boolean(val)
            });
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
    pub(super) fn exec_num(&self, val: i32, job: Job, latest: bool) {
        if latest {
            debug!("set scope value (str expr)");
            job.scope.set_value(if self.is_abstract {
                memory::abstract_number()
            } else {
                memory::number(val)
            });
        } else {
            debug!("dead number expression spoted");
        }
        self.complete_job(job);
    }

    /// Execute a Ref expression statement. Just an expression with a reference.
    pub(super) fn exec_ref(&self, ref_id: &str, cast_as: Option<&str>, job: Job, latest: bool) {
        debug!("process job with single reference {:?}", v);
        if latest {
            if let Some(val) = job.scope.memory.find(ref_id, &job) {
                debug!("store value {:?}", val);
                job.scope.set_value(val);
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
    pub(super) fn exec_function(&self, function: &Function, job: Job, latest: bool) {
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
            job.scope
                .set_value(memory::function(function.clone(), captures));
        } else {
            debug!("dead string expression spoted");
        }
        self.complete_job(job);
    }
}
