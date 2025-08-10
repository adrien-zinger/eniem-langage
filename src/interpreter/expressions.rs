use super::{exec_tree::*, job::*, *};

use std::sync::atomic::Ordering;

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

    /// Execute an expression statement function declaration. Not a call.
    /// Put in memory the captured variable if found.
    pub(super) fn exec_function(&self, function: &Function, job: Job, latest: bool) {
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
