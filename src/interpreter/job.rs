use crate::interpreter::*;

/// After a scope complete (each expression in compound complete).
/// A `WriteJob` is used to describe what the interpreter has to
/// do with the scope memory and the returned value.
///
/// Generally the WriteJob is generated when the user write:
///
/// ```
/// let a = { expr }
/// ```
///
/// See `Interpreter::exec_write` in interpreter::exec.
pub struct WriteJob {
    /// Variable tag. `a` identifier in the example.
    pub tag: String,
    /// Value of the scope. `{ expr }` in the example.
    pub var: BoxVariable,
    /// Variables tags declared in the scope.
    pub decls: Vec<String>,
    /// True if it is the `a := {}` case.
    pub modify: bool,
}

pub enum EJob {
    Expression(Expression),
    /// Builtin function as printf, itoa, i32_add...
    Builtin(Call),
    /// Case 1: One job is blocking and next job is a list of
    ///         expressions. Not managed by the exec function.
    /// Case 2: Abstract interpretation need to wait for input
    ///         before running the function compound statement.
    Expressions(Vec<Expression>),
    /// Write value from box into memory (end scope)
    Write(WriteJob),
    /// Cast check result, execute nexts jobs if BoxVariable true.
    Cast((BoxVariable, Vec<String>)),
    /// Apply cast if check ok. Set the variable into the tag.
    ApplyCast((Arc<Variable>, String)),
    /// Apply cast if check ok. Set variable into current scope value.
    ApplyCastScope(Arc<Variable>),
    /// Free scope
    Delete(Vec<String>),
    /// End of scope
    Empty((BoxVariable, Vec<String>)),
}

#[derive(Clone)]
pub struct Job {
    /// Kind of job it contains.
    pub inner: Arc<EJob>,
    /// Jobs to be executed after completing this one.
    pub next: Option<Arc<EJob>>,
    /// Scope of the current job.
    pub scope: Arc<Scope>,
    /// Related function call of this Job. A parameter
    /// assignation, an external reference assignation (inputs)
    /// or the main compound of a call (output) have a function
    /// call.
    pub(super) fc: Option<Arc<Mutex<FunctionCall>>>,
}
