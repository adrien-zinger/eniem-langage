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
    /// Interprets assignation of a ref to another.
    /// Handling a Ref in execution tree means a variable tag. The function
    /// here need to find in current scope memory a variable with that tag.
    /// If the function failed to find a variable, retry later by rescheduling
    /// the job.
    ///
    /// # Memory
    ///
    /// Assigning a reference to another means that we replace what's in the
    /// box of the left part by what's in the box of the right part. Modifying
    /// the right part after a such action will modify the left part as a side
    /// effect. See `crate::memory` module.
    pub(super) fn assign_ref_statement(
        &self,
        input: &str,
        cast_as: Option<&str>,
        assign: &Assignation,
        job: Job,
    ) {
        debug!("try to assign {} from {input}", assign.var);
        let right_part = job.scope.memory.find(input, &job);
        if right_part.is_none() {
            debug!("retry to assign {} later, {input} not found", assign.var);
            self.schedule(job);
            return;
        }

        let mut right_part = right_part.unwrap();

        if let Some(cast_as) = cast_as {
            if self.is_abstract {
                right_part = memory::add_type(&right_part, cast_as.to_string());
            } else {
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
                    let call = Call {
                        block_on: false,
                        params: vec![],
                        name: cast_as.to_string(),
                        std: StdFunction::default(),
                    };
                    if self
                        .call_statement(&call, job.clone(), false, Some(cast_key), true)
                        .is_ok()
                    {
                        debug!("call cast check function, wait now for {cast_key} to be init");
                        self.schedule(job);
                        return;
                    } else {
                        right_part = memory::add_type(&right_part, cast_as.to_string());
                    }
                }
            }
        }

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
}
