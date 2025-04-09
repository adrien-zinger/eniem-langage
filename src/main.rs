mod exec_tree;
mod interpreter;
mod parser;
mod scopes;
mod tree;
mod memory;

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
    let input = std::fs::read_to_string("main.n").unwrap();
    let ast: Vec<Expression> = expressions(Span::new(&input)).unwrap().1;
    debug!(
        "\n\n --------- ast ------\n\n {:#?} \n\n ----------------------",
        ast
    );
    let mut scopes = scopes::Scopes { errors: vec![] };
    let ast = scopes.check(ast);
    debug!("scopes errors: {:?}", scopes.errors);
    if scopes.errors.len() > 0 {
        for e in scopes.errors {
            eprintln!("{e}");
        }
        return;
    }
    let ast: Vec<exec_tree::Expression> = ast.into_iter().map(|s| s.into()).collect();
    Interpreter::default().run(&ast);
}
