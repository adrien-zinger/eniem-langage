use super::{
    job::{EJob, Job},
    memory::{BoxVariable, Variable},
    Interpreter, WriteJob,
};

use std::sync::{atomic::Ordering, Arc};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

impl Interpreter {
    pub(super) fn exec_empty(&self, job: Job, value: &BoxVariable, decls: &[String]) {
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
            inner: EJob::Delete(decls.to_owned()).into(),
            next: None,
            scope: job.scope.clone(),
            fc: None,
        });
        debug!("complete empty job");
        self.complete_job(job);
    }

    pub(super) fn exec_apply_cast(&self, job: Job, var: Arc<Variable>, dest: String) {
        job.scope.memory.write(dest, var);
        debug!("complete apply cast job");
        self.complete_job(job);
    }

    pub(super) fn exec_cast(&self, job: Job, value: &BoxVariable, decls: &[String]) {
        let cast = unsafe { std::sync::Arc::from_raw(value.load(Ordering::SeqCst)) };

        self.schedule(Job {
            inner: EJob::Delete(decls.to_owned()).into(),
            next: None,
            scope: job.scope.clone(),
            fc: None,
        });

        // do a manual "complete job"
        if job.scope.len.fetch_sub(1, Ordering::SeqCst) == 1 {
            if let Some(job) = &job.scope.job {
                match super::memory::to_boolean(&cast) {
                    Some(true) => {
                        debug!("scheduling apply cast");
                        self.schedule(job.clone());
                    }
                    _ => panic!("failed to cast variable"),
                }
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

    /// Execute the parent Job of a Scope when the scope has to
    /// `write` his value into memory.
    ///
    /// # Abstract
    /// In abstract execution, if the job contains a "function call" then
    /// set the output value of that one. When the output is set, notify
    /// the interpreter that a new resolved function calls has been done.
    /// The function panic if the type of the output doesn't match abstract
    /// types used.
    ///
    /// See `job::WriteJob`.
    pub(super) fn exec_write(&self, job: Job, wr: &WriteJob) {
        let tag = &wr.tag;
        let value = &wr.var;
        let decls = &wr.decls;
        let modify = wr.modify;
        let value = *unsafe { Box::from_raw(value.load(Ordering::SeqCst)) };
        debug!("EJob::Write {:?} into {}", value, tag);
        if self.is_abstract {
            if let Some(fc) = &job.fc {
                let fc = &mut fc.lock().unwrap();
                // check value type
                match &*value {
                    Variable::Abstract(_) => {}
                    Variable::Function(_) => {}
                    _ => panic!("non abstract type"),
                }
                fc.output = value.clone();
                debug!("push new resolved function (Write) id: {}", fc.id);
                self.resolved_function_calls
                    .lock()
                    .unwrap()
                    .insert((fc.id.clone(), fc.inputs.clone()), fc.output.clone());
            }
            job.scope.memory.abstr_write(tag.clone(), value);
        } else if modify {
            job.scope.memory.write_copy(tag.clone(), value);
        } else {
            job.scope.memory.write(tag.clone(), value);
        }
        self.schedule(Job {
            inner: EJob::Delete(decls.clone()).into(),
            next: None,
            scope: job.scope.clone(),
            fc: None,
        });
        self.complete_job(job);
    }
}
