use crate::interpreter::*;
use crate::memory::*;

#[derive(Clone)]
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
    Write(
        (
            String,
            BoxVariable,
            Vec<String>, /* variables declared in the scope */
            bool,        /* modify or not */
        ),
    ),
    /// Free scope
    Delete(Vec<String>),
    /// End of scope
    Empty((BoxVariable, Vec<String>)),
}

#[derive(Clone)]
pub struct Job {
    /// Kind of job it contains.
    pub inner: EJob,
    /// Jobs to be executed after completing this one.
    pub next: Option<EJob>,
    /// Scope of the current job.
    pub scope: Arc<Scope>,
    /// Related function call of this Job. A parameter
    /// assignation, an external reference assignation (inputs)
    /// or the main compound of a call (output) have a function
    /// call.
    pub(super) fc: Option<Arc<Mutex<FunctionCall>>>,
}
