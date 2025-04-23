use crate::exec_tree::*;
use crate::interpreter::*;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicI32, AtomicPtr};
use std::sync::{Arc, Mutex, RwLock};

/// Type detected by the interpreter during abstract execution.
#[derive(PartialEq, Eq, Debug, Hash, Clone, Default)]
pub enum AbstractVariable {
    /// String type.
    String,
    /// I32 type.
    Number,
    /// The type is undefined or is pointless for the type checking.
    /// A type can be replaced by "void" (nothing) when the variable
    /// is never used.
    #[default]
    Uninit,
}

/// Variable tag and a reference to that variable.
type Inputs = Vec<(String, Arc<Variable>)>;

#[derive(Debug)]
pub enum Variable {
    /// Function tree and a list of captured variables.
    Function(Mutex<(Function, Inputs)>),
    /// A mutable String
    String(Mutex<String>),
    /// Nothing, also default variable. Usually it's
    /// used as the default value of a scope.
    Empty,
    /// Empty string type to be used in abstract execution
    Abstract(AbstractVariable),
    /// Signed number on 32 bit is default Number type.
    Number(AtomicI32),
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Variable::Function(a), Variable::Function(b)) => {
                a.lock().unwrap().0 == b.lock().unwrap().0
            }
            (Variable::Abstract(a), Variable::Abstract(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Variable {}

impl Hash for Variable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Variable::Function(f) => f.lock().unwrap().0.hash(state),
            Variable::Abstract(a) => a.hash(state),
            _ => panic!("Variable hashing used outside abstract execution"),
        }
    }
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

/*
Issue 'MemoryPush':
The push is removed because it didn't take into account
that the previous scope wouldn't have already initialized
things that are refered in sub scope.

When we create a new scope, instead of clone the memory
map, we need to know "what is used inside this new scope
which isn't declared inside?". In other words, what is
the externals references in that new scope.


>   Before pushing, we could create a new empty variable
>   with the correct id of what it would be referenced.
>   We can also clone only what would be usefull for the
>   scope.

pub fn push(mem: Arc<Memory>) -> Arc<Memory> {
    if let Ok(m) = mem.map.read() {
        Arc::new(Memory {
            map: m.clone().into(),
        })
    } else {
        panic!("failed to access memory")
    }
}
*/

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

    pub fn get(&self, key: &str) -> Option<Arc<Variable>> {
        if let Ok(vars) = self.map.read() {
            let varbox = vars.get(key);
            if varbox.is_some() {
                return varbox.cloned();
            }
        }
        None
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
                Variable::Abstract(AbstractVariable::String) => {
                    if let Some(v) = mem.get(&key) {
                        if let Variable::Abstract(AbstractVariable::String) = **v {
                        } else {
                            panic!("unexpected type");
                        }
                    }
                }
                Variable::Function(val) => {
                    if let Some(v) = mem.get(&key) {
                        let val = &mut val.lock().unwrap().0;
                        if let Variable::Function(v) = &**v {
                            val.same_as
                                .lock()
                                .unwrap()
                                .push(v.lock().unwrap().0.clone());
                            v.lock()
                                .unwrap()
                                .0
                                .same_as
                                .lock()
                                .unwrap()
                                .push(val.clone());
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

pub fn number(val: i32) -> Arc<Variable> {
    Arc::new(Variable::Number(AtomicI32::new(val)))
}

pub fn abstract_string() -> Arc<Variable> {
    Arc::new(Variable::Abstract(AbstractVariable::String))
}

pub fn abstract_number() -> Arc<Variable> {
    Arc::new(Variable::Abstract(AbstractVariable::Number))
}

pub fn abstract_uninit() -> Arc<Variable> {
    Arc::new(Variable::Abstract(AbstractVariable::Uninit))
}

pub fn function(val: Function, captures: Vec<(String, Arc<Variable>)>) -> Arc<Variable> {
    Arc::new(Variable::Function(Mutex::new((val, captures))))
}
