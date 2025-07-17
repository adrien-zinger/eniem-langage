//! Interpretation of an assignation expression.
use crate::memory::{self, *};

use std::sync::atomic::AtomicU64;
use std::sync::Arc;

use crate::exec_tree;
use crate::interpreter::{job::*, *};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

impl Interpreter {
    /// Interpreter detected a compound assignation.
    ///
    /// `let foo = { ... }`
    ///
    /// A compound assignation must:
    /// 1. Create a new scope for that compound
    ///     - Register all compound declarations for an
    ///       memory management function. (to be defined)
    ///     - Create a Write Job that will put the compound
    ///       result into the variable `foo`. That Job is
    ///       pushed in queue after compound full resolution.
    ///     - Create a new memory that contain an alias to
    ///       references extern (in upper scopes).
    /// 2. Register the compound inner expression list within the
    ///    scope by calling `Interpreter::expressions`.
    fn assign_compound(&self, input: &Arc<exec_tree::Compound>, assign: &Assignation, job: Job) {
        debug!("assignation create a scope");
        let value = BoxVariable::default();
        debug!("scope decls: {:?}", input.decls);
        debug!("scope refs: {:?}", assign.to_assign.refs);
        let new_scope_id = self.new_id();
        let decls = input
            .decls
            .iter()
            .map(|id| format!("{}::{}", id, new_scope_id))
            .collect();

        let job = Job {
            inner: EJob::Write(WriteJob {
                tag: format!("{}::{}", assign.var, job.scope.id),
                var: value.clone(),
                decls,
                modify: assign.modify,
            })
            .into(),
            scope: job.scope,
            next: job.next,
            fc: None,
        };

        let scope = Arc::new(Scope {
            id: new_scope_id,
            len: AtomicU64::new(input.inner.len() as u64),
            memory: job
                .scope
                .memory
                .new(&assign.to_assign.refs, new_scope_id, job.scope.id),
            value,
            job: Some(job),
        });

        self.expressions(&input.inner, scope);
    }

    /// Assign directly a value into memory and complete the Job.
    /// See `Interpreter::complete_job` in common.rs.
    fn assign_str(&self, value: &str, assign: &Assignation, job: Job) {
        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("assign a string to {key}, {value}");
        if self.is_abstract {
            let varbox = memory::abstract_string();
            job.scope.memory.abstr_write(key, varbox);
        } else {
            let varbox = memory::string(value);
            if assign.modify {
                job.scope.memory.write_copy(key, varbox);
            } else {
                job.scope.memory.write(key, varbox);
            }
        }
        self.complete_job(job);
    }

    /// Assign directly a value into memory and complete the Job.
    /// See `Interpreter::complete_job` in common.rs.
    fn assign_num(&self, value: i32, assign: &Assignation, job: Job) {
        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("assign a number to {key}");
        if self.is_abstract {
            let varbox = memory::abstract_number();
            job.scope.memory.abstr_write(key, varbox);
        } else {
            let varbox = memory::number(value);
            if assign.modify {
                job.scope.memory.write_copy(key, varbox);
            } else {
                job.scope.memory.write(key, varbox);
            }
        }
        self.complete_job(job);
    }

    /// Interprets function assignation or declaration.
    ///
    /// This function is called when an assignation with a function
    /// statement as right part is detected. Note that we do not
    /// consider the "value" of the right part.
    ///
    /// ```ignore
    ///    let foo = (){ ... } // function assignation
    ///    let bar = foo       // ref assignation
    ///    let foo = {(){...}} // compound assignation
    /// ```
    ///
    /// - Interpreter must push in scope memory a copy of the execution
    /// tree of targeted function.
    /// - Put an alias to each captured variable into the same memory
    /// region. The captured variable are destroyed from memory only
    /// if all occurence of that variable are destroyed (that one included).
    /// - If any of the captured variable isn't initialized, reschedule the
    /// job.
    ///
    /// # Abstract interpreter
    ///
    /// The abstract interpreter store the function in `Interpreter::functions`
    /// HashMap. Also, the write into memory has a side effect to the exec_tree
    /// itself. Look at the memory module for more information `memory::abstr_write`.
    fn assign_function(&self, input: &exec_tree::Function, assign: &Assignation, job: Job) {
        debug!("Assign a function");
        debug!("function refs: {:?}", assign.to_assign.refs);
        let mut captures = vec![];
        for c in &input.captures {
            if let Some(var) = job.scope.memory.find(c, &job) {
                captures.push((c.clone(), var));
            } else {
                self.schedule(job);
                return;
            }
        }
        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("Write {:?} into {}", input, key);

        if self.is_abstract {
            if let Ok(functions) = &mut self.functions.lock() {
                if !functions.contains_key(&input.id) {
                    functions.insert(input.id.clone(), input.clone());
                }
            }
            job.scope
                .memory
                .abstr_write(key, memory::function(input.clone(), captures));
        } else if assign.modify {
            job.scope
                .memory
                .write_copy(key, memory::function(input.clone(), captures));
        } else {
            job.scope
                .memory
                .write(key, memory::function(input.clone(), captures));
        }
        self.complete_job(job);
    }

    /// Interprets a call statement. Redirect to `Interpreter::call_statement`.
    /// See module interpreter::call.
    fn assign_call(&self, input: &exec_tree::Call, assign: &Assignation, job: Job) {
        debug!("Execute Call statement in assignation");
        let id = format!("{}::{}", assign.var, job.scope.id);
        // TODO use the _ bellow
        let _ = self.call_statement(input, job, false, Some(id), assign.modify);
    }

    /// Same as call but with builtins and libc bindings. See alse `call`.
    fn assign_std_call(&self, input: &exec_tree::Call, assign: &Assignation, job: Job) {
        debug!("Execute StdCall statement in assignation");
        let id = format!("{}::{}", assign.var, job.scope.id);
        self.std_call_statement(input, job, false, Some(id), assign.modify)
    }

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
    fn assign_ref_statement(&self, input: &str, assign: &Assignation, job: Job) {
        debug!("try to assign {} from {input}", assign.var);
        let right_part = job.scope.memory.find(input, &job);
        if right_part.is_none() {
            debug!("retry to assign {} later, {input} not found", assign.var);
            self.schedule(job);
            return;
        }
        let val = right_part.unwrap();
        let key = format!("{}::{}", assign.var, job.scope.id);
        debug!("assign {} from {input}, {:?}", assign.var, val);
        if self.is_abstract {
            job.scope.memory.abstr_write(key, val);
        } else if assign.modify {
            job.scope.memory.write_copy(key, val);
        } else {
            job.scope.memory.write(key, val);
        }
        self.complete_job(job);
    }

    /// Entry point of the interpreter assignation handling.
    ///
    /// ```ignore
    /// let a = /* any */
    /// b = /* any */
    /// ```
    ///
    /// # Inputs
    /// The input Job is generated by `Interpreter::expressions` which is
    /// called whenever the interpreter handle a Compound statement, or
    /// in the entry point of the Interpreter itself (see modules
    /// interpreter::{self, common})
    ///
    /// The `assign` variable is an inner part of the job. Already extracted
    /// because there is a good chance to enter here after a match
    /// expression to EExpression::{Assignation,Definition}.
    pub fn assignation(&self, assign: &Assignation, job: Job) {
        debug!("enter assignation, job scope id {}", job.scope.id);
        match &assign.to_assign.inner {
            EStatement::Compound(input) => self.assign_compound(input, assign, job),
            EStatement::Str(value) => self.assign_str(value, assign, job),
            EStatement::Num(value) => self.assign_num(*value, assign, job),
            EStatement::Function(input) => self.assign_function(input, assign, job),
            EStatement::Call(input) => self.assign_call(input, assign, job),
            EStatement::StdCall(input) => self.assign_std_call(input, assign, job),
            EStatement::Copy(_c) => todo!(),
            EStatement::Ref(input) => self.assign_ref_statement(input, assign, job),
            EStatement::Branch(_branch) => todo!(),
        }
    }
}

#[test]
fn test_assignation_ref() {
    let interpreter = Interpreter::default();
    let memory = Arc::new(Memory::default());
    let varbox = memory::string("hello world");
    memory.write("foo::1".into(), varbox);
    let scope = Arc::new(Scope {
        id: 1,
        job: None,
        len: AtomicU64::new(1),
        memory: memory.clone(),
        value: Default::default(),
    });
    let assignation = Assignation {
        block_on: false,
        var: "bar".into(),
        to_assign: Statement {
            inner: EStatement::Ref("foo".into()),
            refs: Default::default(),
        },
        modify: false,
    };
    let job = Job {
        inner: EJob::Expression(Expression {
            inner: EExpression::Assignation(assignation.clone()),
            latest: true,
        })
        .into(),
        next: None,
        fc: None,
        scope: scope.clone(),
    };
    interpreter.assignation(&assignation, job.clone());
    assert!(memory.find("bar", &job).is_some());
}
