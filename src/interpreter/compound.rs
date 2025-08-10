//! Interpretation of an assignation expression.
use crate::interpreter::{job::*, *};
use exec_tree;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

impl Interpreter {
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
    pub(super) fn assign_compound(
        &self,
        input: &Arc<exec_tree::Compound>,
        assign: &Assignation,
        job: Job,
    ) {
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
}
