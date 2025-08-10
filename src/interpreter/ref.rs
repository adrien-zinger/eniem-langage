//! Interpretation of an assignation expression.
use memory;

use crate::interpreter::{job::*, *};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

impl Interpreter {
    pub(super) fn assign_ref_as(&self, input: &str, cast_as: &str, assign: &Assignation, job: Job) {
        debug!("try to assign {} as {} from {input}", assign.var, cast_as);
        let right_part = job.scope.memory.find(input, &job);
        if right_part.is_none() {
            debug!("retry to assign {} later, {input} not found", assign.var);
            self.schedule(job);
            return;
        }

        let mut right_part = right_part.unwrap();

        let key = format!("{}::{}", assign.var, job.scope.id);
        if self.is_abstract {
            let right_part = memory::add_type(&right_part, cast_as.to_string());
            job.scope.memory.abstr_write(key, right_part);
            return;
        }

        let cast_key = format!("{}::{}", cast_as, job.scope.id);
        if let Some(cast_res) = job.scope.memory.get(&cast_key) {
            if let Some(is_casted) = memory::to_boolean(&cast_res) {
                if is_casted {
                    debug!("cast sucess");
                    right_part = memory::add_type(&right_part, cast_as.to_string());
                } else {
                    panic!("cast error");
                }
            } else {
                debug!("waiting cast check result");
                self.schedule(job);
                return;
            }
        } else {
            debug!("going to define {cast_key}");
            let call = Call {
                block_on: false,
                params: vec![],
                name: cast_as.to_string(),
                std: StdFunction::default(),
            };
            if self
                .call_statement(&call, job.clone(), false, Some(cast_key), true, false)
                .is_ok()
            {
                debug!("call cast check function, wait now for cast_key to be init");
                self.schedule(job);
                return;
            } else {
                debug!("set right part as {cast_as} w/o calling function");
                right_part = memory::add_type(&right_part, cast_as.to_string());
            }
        }

        if assign.modify {
            job.scope.memory.write_copy(key, right_part);
        } else {
            debug!("write {:?} into {key}", right_part);
            job.scope.memory.write(key, right_part);
        }
        self.complete_job(job);
    }

    /// Interprets assignation of a ref to another.
    /// Handling a Ref in execution tree means handling a variable name.
    ///
    /// Example: In `let a = b`, `b` is a ref.
    ///
    /// The function here need to find in current scope memory a variable with that tag.
    /// If the function failed to find a variable, retry later by rescheduling
    /// the job.
    ///
    /// # Memory
    ///
    /// Assigning a reference to another means that we replace what's in the
    /// box of the left part by what's in the box of the right part. Modifying
    /// the right part after a such action will modify the left part as a side
    /// effect. See `crate::memory` module.
    ///
    /// In other words, a variable is immutable since we just assign with no
    /// modification. If a variable is assigned with `:=` operator, then
    /// the value in the left part is changed. The right part can be considered
    /// as cloned.
    pub(super) fn assign_ref(&self, input: &str, assign: &Assignation, job: Job) {
        debug!("try to assign {} from {input}", assign.var);
        let right_part = job.scope.memory.find(input, &job);
        if right_part.is_none() {
            debug!("retry to assign {} later, {input} not found", assign.var);
            self.schedule(job);
            return;
        }

        let right_part = right_part.unwrap();

        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("assign {} from {input}, {:?}", assign.var, right_part);
        if self.is_abstract {
            job.scope.memory.abstr_write(key, right_part);
        } else if assign.modify {
            job.scope.memory.write_copy(key, right_part);
        } else {
            job.scope.memory.write(key, right_part);
        }
        self.complete_job(job);
    }

    /// Execute a Ref expression statement. Just an expression with a reference.
    pub(super) fn exec_ref(&self, ref_id: &str, cast_as: Option<&str>, job: Job, latest: bool) {
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
}
