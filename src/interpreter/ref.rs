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
    /// Takes the right part of the assignation expression, copy the content and
    /// check if the cast would work or not by calling the type's function definition.
    pub(super) fn assign_ref_as(&self, input: &str, cast_as: &str, assign: &Assignation, job: Job) {
        debug!("try to assign {} as {} from {input}", assign.var, cast_as);
        let right_part = job.scope.memory.find(input, &job);
        if right_part.is_none() {
            debug!("retry to assign {} later, {input} not found", assign.var);
            self.schedule(job);
            return;
        }

        let right_part = right_part.unwrap();

        let key = format!("{}::{}", assign.var, job.scope.id);
        if self.is_abstract {
            let right_part = memory::add_type(&right_part, cast_as.to_string());
            job.scope.memory.abstr_write(key, right_part);
            return;
        }

        let copy_key = format!("{}#cast", input);

        let copy = memory::add_type(&right_part, cast_as.to_string());

        let job = Job {
            inner: EJob::ApplyCast((copy.clone(), key)).into(),
            scope: job.scope,
            next: job.next,
            fc: None,
        };

        let scope = Scope {
            id: self.new_id(),
            len: AtomicU64::new(1),
            value: BoxVariable::default(),
            memory: job.scope.memory.clone(),
            job: Some(job),
        };

        let call = Call {
            block_on: false,
            params: vec![Statement {
                inner: EStatement::Ref(copy_key.clone()),
                refs: Default::default(),
            }],
            name: cast_as.to_string(),
            cast: true,
            std: StdFunction::default(),
        };

        let job = Job {
            inner: EJob::Expression(Expression {
                latest: true,
                inner: EExpression::Statement(Statement {
                    inner: EStatement::Call(call),
                    refs: Default::default(),
                }),
            })
            .into(),
            scope: scope.into(),
            next: None,
            fc: None,
        };

        debug!("write {:?} into {copy_key}", copy);
        job.scope
            .memory
            .write(format!("{copy_key}::{}", job.scope.id), copy.clone());
        job.scope.memory.find(&copy_key, &job).unwrap();
        self.schedule(job);
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
