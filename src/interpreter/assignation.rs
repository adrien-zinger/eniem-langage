///! Interpretation of an assignation expression.
use crate::memory::{self, *};

use std::sync::atomic::AtomicU64;
use std::sync::Arc;

use crate::interpreter::*;
use crate::exec_tree;

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
            inner: EJob::Write((
                format!("{}::{}", assign.var, job.scope.id),
                value.clone(),
                decls,
            )),
            scope: job.scope,
            next: job.next,
            fc: None,
        };

        let scope = Arc::new(Scope {
            id: new_scope_id,
            len: AtomicU64::new(input.inner.len() as u64),
            memory: job.scope.memory.new(
                &assign.to_assign.refs,
                new_scope_id,
                job.scope.id,
            ),
            value,
            job: Some(job),
        });

        self.expressions(&input.inner, scope);
	}

	/// Entry point of the interpreter assignation handling.
	pub fn assignation(&self, assign: &Assignation, job: Job) {
        debug!("enter assignation, job scope id {}", job.scope.id);
        match &assign.to_assign.inner {
            EStatement::Compound(input) => self.assign_compound(input, assign, job),
            EStatement::Str(val) => {
                let key = format!("{}::{}", assign.var, job.scope.id);
                debug!("assign a string to {key}, {val}");
                if self.is_abstract {
                    let val = memory::abstract_string();
                    job.scope.memory.abstr_write(key, val);
                } else {
                    let val = memory::string(val);
                    job.scope.memory.write(key, val);
                }
                self.complete_job(job);
            }
            EStatement::Num(val) => {
                let key = format!("{}::{}", assign.var, job.scope.id);
                debug!("assign a number to {key}");
                if self.is_abstract {
                    let val = memory::abstract_number();
                    job.scope.memory.abstr_write(key, val);
                } else {
                    let val = memory::number(*val);
                    job.scope.memory.write(key, val);
                }
                self.complete_job(job);
            }
            EStatement::Function(f) => {
                debug!("Declare a function");
                debug!("function refs: {:?}", assign.to_assign.refs);
                let mut captures = vec![];
                for c in &f.captures {
                    if let Some(var) = job.scope.memory.find(c, &job) {
                        captures.push((c.clone(), var));
                    } else {
                        self.schedule(job);
                        return;
                    }
                }
                let key = format!("{}::{}", assign.var, job.scope.id);
                debug!("Write {:?} into {}", f, key);

                if self.is_abstract {
                    if let Ok(functions) = &mut self.functions.lock() {
                        if !functions.contains_key(&f.id) {
                            functions.insert(f.id.clone(), f.clone());
                        }
                    }
                    job.scope
                        .memory
                        .abstr_write(key, memory::function(f.clone(), captures));
                } else {
                    job.scope
                        .memory
                        .write(key, memory::function(f.clone(), captures));
                }
                self.complete_job(job);
            }
            EStatement::Call(c) => {
                debug!("Execute Call statement in assignation");
                let id = format!("{}::{}", assign.var, job.scope.id);
                self.call_statement(c, job, false, Some(id))
            }
            EStatement::StdCall(call) => {
                let id = format!("{}::{}", assign.var, job.scope.id);
                self.std_call_statement(call, job, false, Some(id))
            }
            EStatement::Copy(_c) => todo!(),
            EStatement::Ref(c) => {
                debug!("try to assign {} from {c}", assign.var);
                let r = job.scope.memory.find(c, &job);
                if r.is_none() {
                    debug!("retry to assign {} later", assign.var);
                    self.schedule(job);
                    return;
                }
                let val = r.unwrap();
                let key = format!("{}::{}", assign.var, job.scope.id);
                debug!("assign {} from {c}, {:?}", assign.var, val);
                if self.is_abstract {
                    job.scope.memory.abstr_write(key, val);
                } else {
                    job.scope.memory.write(key, val);
                }
                self.complete_job(job);
            }
        }
    }
}
