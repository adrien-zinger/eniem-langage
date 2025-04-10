use crate::exec_tree::*;
use crate::interpreter::*;
use std::collections::HashMap;
use std::sync::atomic::AtomicPtr;
use std::sync::{Arc, Mutex, RwLock};

#[derive(Debug)]
pub enum Variable {
    /// Function tree and a list of captured variables.
    Function(Mutex<(Function, Vec<(String, Arc<Variable>)>)>),
    /// A mutable String
    String(Mutex<String>),
    /// Nothing, also default variable. Usually it's
    /// used as the default value of a scope.
    Empty,
    AbstractString,
}

impl Default for Variable {
    fn default() -> Self {
        Self::Empty
    }
}

pub type BoxVariable = Arc<AtomicPtr<Arc<Variable>>>;

#[derive(Default)]
pub struct Memory {
    pub map: RwLock<HashMap<String, Arc<Variable>>>,
}

pub fn push(mem: Arc<Memory>) -> Arc<Memory> {
    if let Ok(m) = mem.map.read() {
        Arc::new(Memory {
            map: m.clone().into(),
        })
    } else {
        panic!("failed to access memory")
    }
}

impl Memory {
    /// Look for a variable in memory. The variable is forced to be in
    /// the current scope job or in a upper scope. If there is no variable
    /// found, it means that the variable is still not initialized.
    pub fn find(&self, var: &str, job: &Job) -> Option<Arc<Variable>> {
        let mut scope = &job.scope;
        loop {
            let key = format!("{}::{}", var, scope.id);
            if let Ok(vars) = self.map.read() {
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

    pub fn write(&self, key: String, value: Arc<Variable>) {
        if let Ok(mem) = &mut self.map.write() {
            mem.insert(key, value);
        } else {
            panic!("failed to access memory");
        }
    }

    pub fn abstr_write(&self, key: String, value: Arc<Variable>) {
        if let Ok(mem) = &mut self.map.write() {
            match &*value {
                Variable::AbstractString => {
                    if let Some(v) = mem.get(&key) {
                        if let Variable::AbstractString = **v {
                        } else {
                            panic!("unexpected type");
                        }
                    }
                }
                Variable::Function(val) => {
                    if let Some(v) = mem.get(&key) {
                        let val = &mut val.lock().unwrap().0;
                        if let Variable::Function(v) = &**v {
                            val.calls = v.lock().unwrap().0.calls.clone();
                        } else {
                            panic!("unexpected type");
                        }
                    }
                }
                _ => {}
            }
            mem.insert(key, value);
        } else {
            panic!("failed to access memory");
        }
    }
}

pub fn string(val: &str) -> Arc<Variable> {
    Arc::new(Variable::String(Mutex::new(val.to_string())))
}

pub fn abstract_string() -> Arc<Variable> {
    Arc::new(Variable::AbstractString)
}

pub fn function(val: Function, captures: Vec<(String, Arc<Variable>)>) -> Arc<Variable> {
    Arc::new(Variable::Function(Mutex::new((val, captures))))
}
