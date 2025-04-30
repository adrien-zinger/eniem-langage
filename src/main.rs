mod builtins;
mod exec_tree;
mod interpreter;
mod libc;
mod memory;
mod parser;
mod scopes;
mod tree;

use interpreter::*;
use parser::*;
use tree::*;

macro_rules! debug {
    ($($rest:tt)*) => {
        #[cfg(feature = "debug_main")]
        std::println!($($rest)*)
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("expected one argument");
    }

    let input = std::fs::read_to_string(&args[1]).unwrap();
    let ast: Vec<Expression> = expressions(Span::new(&input)).unwrap().1;
    debug!(
        "\n\n --------- ast ------\n\n {:#?} \n\n ----------------------",
        ast
    );
    /* dum ast
    use std::io::Write;
    std::fs::File::create("ast")
        .unwrap()
        .write_all(format!("{:#?}", ast).as_bytes()).unwrap();
    */
    let mut scopes = scopes::Scopes::default();
    let ast = scopes.check(ast);
    debug!(
        "\n\n --------- ast scope ------\n\n {:#?} \n\n ----------------------",
        ast
    );
    debug!("scopes errors: {:?}", scopes.errors);
    if !scopes.errors.is_empty() {
        for e in scopes.errors {
            eprintln!("{e}");
        }
        return;
    }
    let ast: Vec<exec_tree::Expression> = ast.into_iter().map(|s| s.into()).collect();
    debug!(
        "\n\n --------- ast exec ------\n\n {:#?} \n\n ----------------------",
        ast
    );
    Interpreter::abstr().run(&ast);
    Interpreter::default().run(&ast);
}
