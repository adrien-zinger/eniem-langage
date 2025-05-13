use std::sync::Arc;

use crate::memory::{self, *};
use std::sync::atomic::Ordering;

pub fn abstract_i32_add(a: BoxVariable, b: BoxVariable) -> Result<BoxVariable, String> {
    match (&*a.load(), &*b.load()) {
        (
            Variable::Abstract(AbstractVariable::Number),
            Variable::Abstract(AbstractVariable::Number),
        ) => Ok(memory::abstract_number()),
        _ => Err("i32_add only accept i32 parameters".to_string()),
    }
}

pub fn i32_add(a: BoxVariable, b: BoxVariable) -> BoxVariable {
    match (&*a.load(), &*b.load()) {
        (Variable::Number(a), Variable::Number(b)) => {
            memory::number(a.load(Ordering::SeqCst) + b.load(Ordering::SeqCst))
        }
        _ => unreachable!("{:?} {:?}", a.load(), b.load()),
    }
}

pub fn abstract_i32_mult(a: BoxVariable, b: BoxVariable) -> Result<BoxVariable, String> {
    match (&*a.load(), &*b.load()) {
        (
            Variable::Abstract(AbstractVariable::Number),
            Variable::Abstract(AbstractVariable::Number),
        ) => Ok(memory::abstract_number()),
        _ => Err("i32_mult only accept i32 parameters".to_string()),
    }
}

pub fn i32_mult(a: BoxVariable, b: BoxVariable) -> BoxVariable {
    match (&*a.load(), &*b.load()) {
        (Variable::Number(a), Variable::Number(b)) => {
            memory::number(a.load(Ordering::SeqCst) * b.load(Ordering::SeqCst))
        }
        _ => unreachable!(),
    }
}
