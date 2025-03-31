use crate::exec_tree::*;

use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

#[derive(Clone)]
enum EJob {
    Expressions(Vec<Expression>),
    Expression(Expression),
    Write((String, Arc<AtomicPtr<Option<String>>>)),
    Empty,
}

#[derive(Clone)]
struct Job {
    inner: EJob,
    next: Option<EJob>,
    scope: Arc<Scope>,
}

struct Scope {
    id: u64,
    len: AtomicU64,
    value: Arc<AtomicPtr<Option<String>>>,
    /// Tags that are initialized inside that scope.
    decls: Vec<String>,
    /// Parent job
    job: Option<Job>,
}

#[derive(Default)]
pub struct Interpreter {
    jobs: Arc<Mutex<Vec<Job>>>,
    counter: AtomicU64,
}

impl Interpreter {
    fn new_id(&self) -> u64 {
        self.counter.fetch_add(1, Ordering::SeqCst)
    }

    /// public access to interpreter
    pub fn run(&self, input: &[Expression]) {
        debug!("start interpreter");

        self.expressions(
            input,
            Arc::new(Scope {
                id: self.new_id(),
                len: Default::default(),
                value: Default::default(),
                decls: vec![],
                job: None,
            }),
        );

        loop {
            let job = if let Ok(jobs) = &mut self.jobs.lock() {
                if let Some(job) = jobs.pop() {
                    job.clone()
                } else {
                    return;
                }
            } else {
                continue;
            };
            self.exec(job);
        }
    }

    fn schedule(&self, job: Job) {
        if let Ok(jobs) = &mut self.jobs.lock() {
            jobs.push(job);
        }
    }

    /// Create jobs for a list of expressions
    fn expressions(&self, exprs: &[Expression], scope: Arc<Scope>) {
        let mut index = 0;
        for expr in exprs {
            index = index + 1;
            let blocking;
            let mut job = match &expr.inner {
                EExpression::Statement(input) => {
                    blocking = input.is_blocking();
                    Job {
                        inner: EJob::Expression(expr.clone()),
                        next: None,
                        scope: scope.clone(),
                    }
                }
                EExpression::Assignation(input) => {
                    blocking = input.block_on;
                    Job {
                        inner: EJob::Expression(expr.clone()),
                        next: None,
                        scope: scope.clone(),
                    }
                }
                EExpression::Declaration(input) => {
                    blocking = input.block_on;
                    Job {
                        inner: EJob::Expression(expr.clone()),
                        next: None,
                        scope: scope.clone(),
                    }
                }
            };

            if blocking {
                job.next = Some(EJob::Expressions(exprs[index..].to_vec()));
                self.schedule(job);
                break;
            } else {
                self.schedule(job);
            }
        }
    }

    fn assignation(&self, assign: &Assignation, job: Job) {
        match &assign.to_assign.inner {
            EStatement::Compound(input) => {
                debug!("assignation create a scope");
                let value = Arc::<AtomicPtr<Option<String>>>::default();
                let job = Job {
                    inner: EJob::Write((format!("{}_{}", assign.var, job.scope.id), value.clone())),
                    scope: job.scope,
                    next: job.next,
                };
                let scope = Arc::new(Scope {
                    id: self.new_id(),
                    len: AtomicU64::new(input.inner.len() as u64),
                    value,
                    decls: vec![], // todo should already be in the exec tree.
                    job: Some(job),
                });
                self.expressions(&input.inner, scope);
            }
            EStatement::Str(val) => {
                debug!("Write {} into {}_{}", val, assign.var, job.scope.id);
                if 1 == job.scope.len.fetch_sub(1, Ordering::SeqCst) {
                    debug!("Some scope ends with a str");
                    self.schedule(job.scope.job.clone().unwrap());
                }
                if let Some(next) = job.next {
                    match next {
                        EJob::Expressions(exprs) => self.expressions(&exprs, job.scope),
                        _ => unreachable!(),
                    }
                }
            },
			EStatement::Function(_f) => todo!(),
			EStatement::Call(_c) => todo!(),
			EStatement::Copy(_c) => todo!(),
			EStatement::Ref(_c) => todo!(),
        }
    }

    fn exec(&self, job: Job) {
        match &job.inner {
            EJob::Expression(expr) => {
                let latest = expr.latest;
                match &expr.inner {
                    EExpression::Statement(stat) => {
                        match &stat.inner {
                            EStatement::Compound(input) => {
                                debug!("create a new scope from a scope");
                                let value = if latest {
                                    job.scope.value.clone()
                                } else {
                                    Default::default()
                                };
                                let compound = Job {
                                    inner: EJob::Empty,
                                    next: job.next,
                                    scope: job.scope,
                                };
                                let scope = Arc::new(Scope {
                                    id: self.new_id(),
                                    len: AtomicU64::new(input.inner.len() as u64),
                                    value,
                                    decls: vec![], // todo should already be in the exec tree.
                                    job: Some(compound),
                                });
                                self.expressions(&input.inner, scope);
                            }
                            EStatement::Str(val) => {
                                if latest {
                                    let val = Box::new(Some(val.to_owned()));
                                    job.scope.value.store(Box::into_raw(val), Ordering::SeqCst);
                                }

                                if 1 == job.scope.len.fetch_sub(1, Ordering::SeqCst) {
                                    debug!("some scope end here");
                                    self.schedule(job.scope.job.clone().unwrap());
                                }
                            }
                            EStatement::Call(_call) => {
                                // read call.name in local, it should be possible
                                // to interpret it as a function.
								todo!()
                            },
							EStatement::Copy(_v) => todo!(),
							EStatement::Ref(_v) => todo!(),
							EStatement::Function(_v) => todo!()
                        }
                    }
                    EExpression::Assignation(assignation) => {
                        self.assignation(&assignation, job.clone());
                    }
                    EExpression::Declaration(assignation) => {
                        self.assignation(&assignation, job.clone());
                    }
                }
            }
            EJob::Write((tag, value)) => {
                let value = unsafe { Box::from_raw(value.load(Ordering::SeqCst)) };
                if let Some(val) = *value {
                    debug!("write {} into {}", val, tag);
                } else {
                    debug!("Nothing to write");
                }

                if 1 == job.scope.len.fetch_sub(1, Ordering::SeqCst) {
                    debug!("some scope ends");
                    if let Some(job) = &job.scope.job {
                        self.schedule(job.clone());
                    }
                }

                if let Some(next) = job.next {
                    match next {
                        EJob::Expressions(exprs) => self.expressions(&exprs, job.scope),
                        _ => unreachable!(),
                    }
                }
            }
            EJob::Empty => {
                if 1 == job.scope.len.fetch_sub(1, Ordering::SeqCst) {
                    debug!("some scope ends");
                    if let Some(job) = &job.scope.job {
                        self.schedule(job.clone());
                    }
                }
                if let Some(next) = job.next {
                    match next {
                        EJob::Expressions(exprs) => self.expressions(&exprs, job.scope),
                        _ => unreachable!(),
                    }
                }
            }
            EJob::Expressions(_) => panic!("batch execution not covered"),
        }
    }
}
