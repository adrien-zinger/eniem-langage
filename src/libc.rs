extern crate libc as llibc;

use llibc::{printf /* fflush */};

use std::ffi::CString;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::memory::Variable;

extern "C" {
    // static mut stdout: *mut llibc::FILE;
}

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
                    _ => {
                        unreachable!("not managed");
                    }
                }
            }
        }
        _ => panic!("unmatch printf param len"),
    }
}
