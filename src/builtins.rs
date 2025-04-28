use std::sync::Arc;

use crate::memory::{self, *};
use std::sync::atomic::Ordering;

pub fn abstract_i32_add(a: Arc<Variable>, b: Arc<Variable>) -> Result<Arc<Variable>, String> {
    match (&*a, &*b) {
        (
            Variable::Abstract(AbstractVariable::Number),
            Variable::Abstract(AbstractVariable::Number),
        ) => Ok(memory::abstract_number()),
        _ => Err("i32_add only accept i32 parameters".to_string()),
    }
}

pub fn i32_add(a: Arc<Variable>, b: Arc<Variable>) -> Arc<Variable> {
    match (&*a, &*b) {
        (Variable::Number(a), Variable::Number(b)) => {
            memory::number(a.load(Ordering::SeqCst) + b.load(Ordering::SeqCst))
        }
        _ => unreachable!(),
    }
}

pub fn abstract_i32_mult(a: Arc<Variable>, b: Arc<Variable>) -> Result<Arc<Variable>, String> {
    match (&*a, &*b) {
        (
            Variable::Abstract(AbstractVariable::Number),
            Variable::Abstract(AbstractVariable::Number),
        ) => Ok(memory::abstract_number()),
        _ => Err("i32_mult only accept i32 parameters".to_string()),
    }
}

pub fn i32_mult(a: Arc<Variable>, b: Arc<Variable>) -> Arc<Variable> {
    match (&*a, &*b) {
        (Variable::Number(a), Variable::Number(b)) => {
            memory::number(a.load(Ordering::SeqCst) * b.load(Ordering::SeqCst))
        }
        _ => unreachable!(),
    }
}
