use crate::exec_tree::*;

use std::collections::HashMap;
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_interpreter")]
        std::println!($($rest)*)
    }
}

type BoxType = Arc::<AtomicPtr::<Arc::<Mutex::<String>>>>;

#[derive(Clone)]
enum EJob {
    Expressions(Vec<Expression>),
    Expression(Expression),
    Write((String, BoxType)),
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
    value: BoxType,
    /// Tags that are initialized inside that scope.
    decls: Vec<String>,
    /// Parent job
    job: Option<Job>,
}

#[derive(Default)]
pub struct Interpreter {
    jobs: Arc<Mutex<Vec<Job>>>,
    counter: AtomicU64,
    variables: Arc<Mutex<HashMap<String, Arc<Mutex<String>>>>>,
}

impl Interpreter {
    fn new_id(&self) -> u64 {
        self.counter.fetch_add(1, Ordering::SeqCst)
    }

    fn find(&self, var: &str, job: &Job) -> Option<Arc<Mutex<String>>> {
        let mut scope = &job.scope;
        loop {
            let key = format!("{}::{}", var, scope.id);
            debug!("look at: {}", key);
            if let Ok(vars) = self.variables.lock() {
                let varbox = vars.get(&key);
                if varbox.is_some() {
                    return varbox.cloned();
                }
            }

            if let Some(job) = &scope.job {
                scope = &job.scope;
            } else {
                return None;
            }
        }
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

    fn schedule_later(&self, job: Job) {
        if let Ok(jobs) = &mut self.jobs.lock() {
            jobs.insert(0, job);
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
                let value = Arc::<AtomicPtr::<Arc::<Mutex::<String>>>>::default();
                let job = Job {
                    inner: EJob::Write((
                        format!("{}::{}", assign.var, job.scope.id),
                        value.clone(),
                    )),
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
                if let Ok(vars) = &mut self.variables.lock() {
                    let id = format!("{}::{}", assign.var, job.scope.id);
                    debug!("Write {} into {}", val, id);
                    vars.insert(id, Arc::new(Mutex::new(val.clone())));
                }
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
            }
            EStatement::Function(_f) => todo!(),
            EStatement::Call(_c) => todo!(),
            EStatement::Copy(_c) => todo!(),
            EStatement::Ref(c) => {
				debug!("try to assign {} from {c}", assign.var);
                let r = self.find(c, &job);
                if r.is_none() {
                    self.schedule_later(job);
					return;
                }
				let val = r.unwrap();
				if let Ok(vars) = &mut self.variables.lock() {
                    let id = format!("{}::{}", assign.var, job.scope.id);
					debug!("assign {} from {c}! {:?}", assign.var, val);
                    vars.insert(id, val);
                }
				if 1 == job.scope.len.fetch_sub(1, Ordering::SeqCst) {
                    self.schedule(job.scope.job.clone().unwrap());
                }
                if let Some(next) = job.next {
                    if let EJob::Expressions(exprs) = next {
                    	self.expressions(&exprs, job.scope);
                    }
                }
            }
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
									let boxed = Box::new(Arc::new(Mutex::new(val.clone())));
									job.scope.value.store(Box::into_raw(boxed), Ordering::SeqCst);
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
                            }
                            EStatement::Copy(_v) => todo!(),
                            EStatement::Ref(v) => {
                                if latest {
                                    if let Some(val) = self.find(&v, &job) {
										let boxed = Box::new(val);
										job.scope.value.store(Box::into_raw(boxed), Ordering::SeqCst);
                                    } else {
                                        self.schedule_later(job);
                                        return;
                                    }
                                }
                                if 1 == job.scope.len.fetch_sub(1, Ordering::SeqCst) {
                                    self.schedule(job.scope.job.clone().unwrap());
                                }
                            }
                            EStatement::Function(_v) => todo!(),
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
                if let Ok(vars) = &mut self.variables.lock() {
					let value =  *unsafe { Box::from_raw(value.load(Ordering::SeqCst)) };
					debug!("EJob::Write {:?} into {}", value, tag);
                    vars.insert(tag.clone(), value);
                }

                if 1 == job.scope.len.fetch_sub(1, Ordering::SeqCst) {
                    if let Some(job) = &job.scope.job {
                        self.schedule(job.clone());
                    }
                }
                if let Some(next) = job.next {
                    if let EJob::Expressions(exprs) = next {
                        self.expressions(&exprs, job.scope);
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
