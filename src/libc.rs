extern crate libc as llibc;

use llibc::{printf /* fflush */};

use std::ffi::{c_int, CString};
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::memory::{self, AbstractVariable, Variable};

extern "C" {
    // static mut stdout: *mut llibc::FILE;
}

pub fn abstract_atoi(arg: Arc<Variable>) -> Result<Arc<Variable>, String> {
    match &*arg {
        Variable::String(_) => Ok(memory::abstract_number()),
        Variable::Abstract(AbstractVariable::String) => Ok(memory::abstract_number()),
        _ => Err("atoi expect a string as input".to_string()),
    }
}

pub fn atoi(arg: Arc<Variable>) -> Arc<Variable> {
    match &*arg {
        Variable::String(s) => {
            let s = s.lock().unwrap().clone();
            let s = CString::new(s).unwrap();
            memory::number(unsafe { llibc::atoi(s.as_ptr()) })
        }
        _ => unreachable!(),
    }
}

/*
Printf Parallel Issue:

It's up to the implementation of the user platform to tell
that printf is or isn't thread safe. Using GNU to develop
the interpreter prototype, printf looks safe in multithreading.
But we should not depend on GNU and its particullarities.

We have to think about a mechanism that create a fence
for any file descriptor IO, including standard outputs.

A lock-free mechanism is preferable.
*/

/// Bind to libc printf function
pub fn builtin_printf(args: &[Arc<Variable>]) -> i32 {
    // rust interpreter can't match variadics. Just 1 argument
    // is accepted right now.
    match args.len() {
        1 => {
            let string = match &*args[0] {
                Variable::String(string) => string.lock().unwrap().clone(),
                _ => {
                    unreachable!("not managed");
                }
            };
            let s = CString::new(string).unwrap();
            unsafe { printf(s.as_ptr()) }
        }
        2 => {
            let string = match &*args[0] {
                Variable::String(string) => string.lock().unwrap().clone(),
                _ => {
                    unreachable!("not managed");
                }
            };
            let s = CString::new(string).unwrap();
            unsafe {
                match &*args[1] {
                    Variable::String(string) => {
                        let cstring = CString::new(string.lock().unwrap().clone()).unwrap();
                        printf(s.as_ptr(), cstring.as_ptr())
                    }
                    Variable::Number(number) => printf(s.as_ptr(), number.load(Ordering::SeqCst)),
                    Variable::Boolean(boolean) => {
                        printf(s.as_ptr(), boolean.load(Ordering::SeqCst) as c_int)
                    }
                    _ => {
                        unreachable!("not managed");
                    }
                }
            }
        }
        _ => panic!("unmatch printf param len"),
    }
}
