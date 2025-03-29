mod interpreter;
mod tree;
mod exec_tree;
mod parser;

use tree::*;
use interpreter::*;
use parser::*;

#[test]
fn test_interpreter() {
	let input = "let a = \"abc\";";
	let ast: Vec<Expression> = expressions(Span::new(input)).unwrap().1;
	let ast: Vec<exec_tree::Expression> = ast.into_iter().map(|s| s.into()).collect();
	Interpreter::default().run(&ast);
}

fn main() {
    let input = std::fs::read_to_string("main.n").unwrap();
	let ast: Vec<Expression> = expressions(Span::new(&input)).unwrap().1;
	let ast: Vec<exec_tree::Expression> = ast.into_iter().map(|s| s.into()).collect();
	Interpreter::default().run(&ast);
}
